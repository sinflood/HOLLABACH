open Ast
open Printf

module StringMap = Map.Make(String)
let header  = "Timing Resolution (pulses per quarter note)\n4\n\n"

(* creates the CSV header*)
let writeHeader fname insts = let oc= open_out_gen [Open_creat; Open_wronly; Open_append;
Open_text] 0o666 fname in
fprintf oc "%s%s" header insts; close_out oc

(*writes out the measure contents to the CSV out file*)
let writeOutput bpm k  fname trackNum= let oc = open_out_gen [Open_creat; Open_wronly;
Open_append; Open_text] 0o666 fname in 
(* returns the duration of the note in ticks*)
let getLen notestr =
        match notestr.[ (String.length notestr) -1] with
        '1' -> 16 (* whole note*)
        | '2' ->8
        | '4' -> 4
        | '8' -> 2
        |  '6' -> 1 (*16th note*)
        | _ -> raise (Failure("Invalid note length"))
in
let getMod noter = 
        match noter.[1] with
        'b' -> -1
        | '#' -> 1
        | _ -> 0
in
let getOctave noter =
        if getMod noter != 0 then
                ((int_of_char noter.[2] -48)* 12 + 12) (*octave 0 starts at 24*)
        else
                (int_of_char noter.[1] -48)* 12 + 12
in
(*translates the letter note to the value expected by CSV2MIDI *)
let getNote noter =  
        match noter.[0] with
        'A' -> 9 + getOctave noter
        | 'B' -> 11 + getOctave noter
        | 'C' -> 0 + getOctave noter
        | 'D' -> 2 + getOctave noter
        | 'E' -> 4 + getOctave noter
        | 'F' -> 5 + getOctave noter
        | 'G' -> 7 + getOctave noter
        | _ -> raise (Failure ("Not a note value!"))
   
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
                let noteOff = (float_of_int m.measLen /. float_of_int
                (List.length m.body)) *. 4.0 in
                List.fold_left (fun note_num naw ->
                        match naw with
                        (*Note(n) -> fprintf oc "%s\n" (getNoteString naw (off+note_num)); note_num + noteOff
                        |*) Chord(cr) -> List.iter (fun nat -> printNote nat
                        (off+note_num)) cr; note_num + int_of_float noteOff
                        | _ -> raise(Failure("Trying to print something that
                        isn't a measure!"))
        )  0 m.body; off + (m.measLen * 4)
        | _ -> raise (Failure("ERROR, NOT A MEASURE!"))
in
List.fold_left printMeasure 0 k ; 
close_out oc

(*contains all of the encountered varables and values for them *)
let  vars = ref StringMap.empty

let compile stmts outfile = 
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
        | "Acoustic Bass" -> "32"
        | "Alto Sax" -> "65"
        | "Bag Pipe" -> "109"
        | "Flute" -> "73"
        | "Piano" -> "0"
        | "Tenor Sax" -> "66"
        | "Trombone" -> "77"
        | "Trumpet" -> "56"
        | "Violin" -> "40"
        | _ -> "0"
in
let getInstrumentLine tracks =
        List.fold_left (fun s c ->
                ("Instrument,"  ^ (getInstr c.inst) ^ ",,") ^ s ) "" tracks
in
let rec getColumnNames inst_count =
        if inst_count > 0 then
                "Tick,Note(0-127),Velocity(0-127),Length," ^getColumnNames (inst_count -1)
        else
               "" 
in

let currMeasLen = ref 4
in
let processMeasure me outp =
        let meas = Measure(me) in
        match meas with
        Measure(m) -> if (List.mem (List.length m.body) [1; 2; 4; 8; 16; 32]) then
                ((vars := StringMap.add m.id m !vars); m.measLen<- !currMeasLen;
                Measure(m) :: outp) else raise(Failure("Malformed measure. Incorrect number of notes/chords
        in the measure."))
        
        |_ -> outp
in
        let rec exec out env = match env with
        Measure(m) -> (processMeasure m out)
                | MeasureLen(m) ->  currMeasLen := m; out
                | Loop(c, b) -> let rec callLoop i body = 
                        if i>0 then
                        (List.fold_left exec [] body) @ (callLoop (i-1) body)
                        else [] 
                in (List.rev (callLoop (eval c) b)) @ out
                | Id(i) -> Measure((StringMap.find i !vars)) :: out
                | If(c,b1,b2) -> out
                | a -> out
        in
        writeHeader outfile ((getInstrumentLine stmts) ^ "\n\n" ^ getColumnNames
        (List.length stmts) ^ "\n");
        List.fold_left (fun i c ->
        let comped = List.fold_left exec [] (List.rev c.body)
        in
        writeOutput 120 (List.rev comped) outfile i; i+1 ) 0 stmts;() (* TODO why do we need to reverse it?*)
