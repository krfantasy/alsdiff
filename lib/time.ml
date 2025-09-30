type time_position = {
  bars : int;
  beats : int;
  sixteenths : int;
}

type time_signature = {
  numerator : int;
  denominator : int;
}

let get_real_time bpm time_sig time_pos =
  let seconds_per_beat = 60.0 /. bpm in
  let total_beats =
    (float_of_int time_pos.bars *. float_of_int time_sig.numerator) +.
    float_of_int time_pos.beats +.
    (float_of_int time_pos.sixteenths /. 16.0)
  in
  total_beats *. seconds_per_beat
