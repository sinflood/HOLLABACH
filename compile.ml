open Ast
open Printf

module StringMap = Map.Make(String)
let header  = "Timing Resolution (pulses per quarter note)\n4\n\n"

(* creates the CSV header*)
let writeHeader fname insts = let oc= open_out_gen [Open_creat; Open_wronly; Open_append;
Open_text] 0o666 fname in
fprintf oc "%s%s" header insts; close_out oc

(*writes out the measure contents to the CSV out file. Expects the input to be a
 * list of measures, the outfile name and an integer representing the
 * instrument's position. *)
let writeOutput k  fname trackNum= let oc = open_out_gen [Open_creat; Open_wronly;
Open_append; Open_text] 0o666 fname in 
(* returns the duration of the note in ticks*)
let getLen notestr =
        match notestr.[ (String.length notestr) -1] with
        '1' -> 16 (* whole note*)
        | '2' ->8
        | '4' -> 4
        | '8' -> 2
        |  '6' -> 1 (*16th note*)
        | _ -> raise (Failure("Invalid note length on " ^ notestr))
in
(*adjusts pitch for sharps and flats*)
let getMod noter = 
        match noter.[1] with
        'b' -> -1
        | '#' -> 1
        | _ -> 0
in
(* returns the modifier for the note octave *)
let getOctave noter =
        if getMod noter != 0 then
                ((int_of_char noter.[2] -48)* 12 + 12)
        else
                (int_of_char noter.[1] -48)* 12 + 12
in
(*translates the letter note to the integer value expected by CSV2MIDI. Includes
 * Octave. *)
let getNote noter =  
        match noter.[0] with
        'A' -> 9 + getOctave noter
        | 'B' -> 11 + getOctave noter
        | 'C' -> 0 + getOctave noter
        | 'D' -> 2 + getOctave noter
        | 'E' -> 4 + getOctave noter
        | 'F' -> 5 + getOctave noter
        | 'G' -> 7 + getOctave noter
        | _ -> raise (Failure ("Not a note value! Note: " ^ noter))
   
in
(*Adds empty columns for previous tracks to the row*)
let rec getPrefix k = 
        if k = 0 then""
        else ",,,,"^ getPrefix (k-1)
in
(* Returns the CSV line for a note*)
let getNoteString no offset = 
         match no with
         Note(n) -> getPrefix trackNum ^ string_of_int offset ^ "," ^ string_of_int ((getNote n) +
         (getMod n)) ^ "," ^ string_of_int 80 ^ "," ^ string_of_int (getLen n)
in
(*prints a note to the CSV file*)
let printNote naw offset=
        match naw with
        Note(n) -> if n.[0] != 'R' then
                fprintf oc "%s\n" (getNoteString naw offset)
in
(* prints a whole measure to the CSV file*)
let printMeasure off n =
        match n with
        Measure(m) -> 
                let noteOff = (float_of_int m.timesig /. float_of_int
                (List.length m.body)) *. 4.0 in
                List.fold_left (fun note_num naw ->
                        match naw with
                        (*Note(n) -> fprintf oc "%s\n" (getNoteString naw (off+note_num)); note_num + noteOff
                        |*) Chord(cr) -> List.iter (fun nat -> printNote nat
                        (off+note_num)) cr; note_num + int_of_float noteOff
                        | _ -> raise(Failure("Trying to print something that
                        isn't a measure!"))
        )  0 m.body; off + (m.timesig * 4)
        | _ -> raise (Failure("ERROR, NOT A MEASURE! value:" ^
        string_of_stmt n))
in
List.fold_left printMeasure 0 k ; 
close_out oc

(*contains all of the encountered varables and values for them *)
let  vars = ref StringMap.empty

(*This function is the main compiler and is called by hollabach.ml. It will
 * evaluate/expand the input stmts and write out the bytecode to the output file
 * outfile*)
let compile stmts outfile = 
        (*evaluates expressions*)
        let rec eval env = match env with
                Literal (l) -> l
               (* | Note(n) -> Note(n), env*)
                | _ -> raise (Failure("Currently only handles literals for
                conditionals.")) 
        in
        (*returns the integer representation expected by CSV2Midi for an instrument*) 
        let getInstr i =
        match i with
        "Banjo" -> "105"
        | "Clarinet" -> "71"
        | "AcousticBass" -> "32"
        | "AltoSax" -> "65"
        | "BagPipe" -> "109"
        | "Flute" -> "73"
        | "Piano" -> "0"
        | "TenorSax" -> "66"
        | "Trombone" -> "77"
        | "Trumpet" -> "56"
        | "Violin" -> "40"
        | _ -> "0"
in
 (*returns the line for declaring instruments in CSV2MIDI *)
let getInstrumentLine tracks =
        List.fold_left (fun s c ->
                ("Instrument,"  ^ (getInstr c.instStr) ^ "," ^ c.instStr ^ ",") ^ s ) "" tracks
in
(*returns the string for the column names in the note declaration section of
 * CSV2MIDI *)
let rec getColumnNames inst_count =
        if inst_count > 0 then
                "Tick,Note(0-127),Velocity(0-127),Length," ^getColumnNames (inst_count -1)
        else
               "" 
in

(* the current time signature value during execution. This can be modified by
 * encoutering TimeSig statements *)
let currTimeSig = ref 4
in
(*helper function for validating and processing a Measure statement. Will check
 * if the length is valid. If true, it will add itself to the variable list,
 * alter it's Time Signature and append itself to the output *)
let processMeasure me outp =
        let meas = Measure(me) in
        match meas with
        Measure(m) -> if (List.mem (List.length m.body) [0;1; 2; 3; 4; 8; 16; 32]) then
                ((vars := StringMap.add m.id m !vars); m.timesig<- !currTimeSig;
                Measure(m) :: outp) else raise(Failure("Malformed measure. Incorrect number of notes/chords
in the measure. Count:" ^ string_of_int (List.length m.body)))
        
        |_ -> outp
in
        (*evaluate a statement and return the updated measure progression *)
        let rec exec ite out env = match env with
                Measure(m) -> (processMeasure m out)
                | TimeSig(m) ->  currTimeSig := m; out
                | Loop(c, b) -> let rec callLoop i body = 
                        if i>=0 then
                            (List.fold_left (exec i) [] body) @ (callLoop (i-1)
                            body)
                        else [] 
                        in   (callLoop ((eval c)-1) b) @ out
                | Id(i) -> Measure((StringMap.find i !vars)) :: out
                | If(i,b,eb) -> if (eval i) = ite then
                        (List.fold_left (exec ite) [] b) @ out 
                else (List.fold_left (exec ite) [] eb) @ out
                | a -> out
        in
        writeHeader outfile ((getInstrumentLine (List.rev stmts)) ^ "\n\n" ^ getColumnNames
        (List.length stmts) ^ "\n");
        List.fold_left (fun i c ->
        let comped = List.fold_left (exec 0) [] c.body (*(List.rev c.body)*)
        in
        writeOutput (List.rev comped) outfile i; i+1 ) 0 stmts;() (* TODO why do we need to reverse it?*)
