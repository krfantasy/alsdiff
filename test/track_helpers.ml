open Alsdiff_live


let make_generic_param name value =
  {
    Device.GenericParam.name = name;
    value = value;
    automation = 0;
    modulation = 0;
    mapping = None;
  }


let make_mixer volume pan =
  {
    Track.Mixer.volume = make_generic_param "Volume" (Device.Float volume);
    pan = make_generic_param "Pan" (Device.Float pan);
    mute = make_generic_param "On" (Device.Bool false);
    solo = make_generic_param "SoloSink" (Device.Bool false);
    sends = [];
  }


let make_empty_routing_set () =
  let make_routing route_type =
    {
      Track.Routing.route_type;
      target = "";
      upper_string = "";
      lower_string = "";
    }
  in
  {
    Track.RoutingSet.audio_in = make_routing Track.Routing.AudioIn;
    audio_out = make_routing Track.Routing.AudioOut;
    midi_in = make_routing Track.Routing.MidiIn;
    midi_out = make_routing Track.Routing.MidiOut;
  }


let make_main_mixer () =
  let base = make_mixer 1.0 0.0 in
  {
    Track.MainMixer.base;
    tempo = make_generic_param "Tempo" (Device.Float 120.0);
    time_signature = make_generic_param "TimeSignature" (Device.Int 4);
    crossfade = make_generic_param "CrossFade" (Device.Float 1.0);
    global_groove = make_generic_param "GlobalGroove" (Device.Float 0.0);
  }

(* ---- Projector-context fixtures (shared by test_projector_context and
   test_view_model) ---- *)

let make_note id note =
  { Clip.MidiNote.id; note; time = 0.0; duration = 1.0; velocity = 100.0;
    off_velocity = 64.0 }

let make_clip id notes =
  { Clip.MidiClip.id; name = "Clip"; start_time = 0.0; end_time = 4.0;
    loop = { Clip.Loop.start_time = 0.0; end_time = 4.0; on = true };
    signature = { Clip.TimeSignature.numer = 4; denom = 4 };
    notes }

let make_event id time =
  { Automation.EnvelopeEvent.id; time; value = Automation.FloatEvent 0.5;
    curve = None }

let make_automation id target events =
  { Automation.id; target; events }

let make_midi_track ?(clips = []) ?(automations = []) id name =
  { Track.MidiTrack.id; name; current_name = name; group_id = -1;
    clips; automations; devices = [];
    mixer = make_mixer 0.7 (-0.3);
    routings = make_empty_routing_set () }

let make_return_track id name =
  { Track.AudioTrack.id; name; current_name = name; group_id = -1;
    clips = []; automations = []; devices = [];
    mixer = make_mixer 0.5 0.1;
    routings = make_empty_routing_set () }

let make_main_track ?(automations = []) () =
  { Track.MainTrack.name = "Main"; current_name = "Main";
    automations; devices = [];
    mixer = make_main_mixer ();
    routings = make_empty_routing_set () }

(* Hand-built Liveset for projector tests: [main] and [tracks] are the only
   meaningful fields; the rest are inert defaults. *)
let make_test_liveset ~(main : Track.MainTrack.t) ~(tracks : Track.t list) : Liveset.t =
  {
    Liveset.name = "Test";
    version = { Liveset.Version.major = "11"; minor = "0"; revision = "0" };
    creator = "";
    tracks;
    returns = [];
    main = Track.Main main;
    locators = [];
    pointees = Liveset.IntHashtbl.create 16;
  }
