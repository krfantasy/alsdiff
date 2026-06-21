type note_display_style = Sharp | Flat [@@deriving yojson, jsonschema]

let note_display_style_to_json x = Yojson.Safe.to_basic (note_display_style_to_yojson x)

type time_format = QuarterNotes | BeatTime | RealTime [@@deriving yojson, jsonschema]

let time_format_to_json x = Yojson.Safe.to_basic (time_format_to_yojson x)

let time_format_equal a b = match a, b with
  | QuarterNotes, QuarterNotes -> true
  | BeatTime, BeatTime -> true
  | RealTime, RealTime -> true
  | _ -> false

type field_value =
  | Fint of int
  | Ffloat of float
  | Fbool of bool
  | Fstring of string

let int_value x = Fint x
let float_value x = Ffloat x
let bool_value x = Fbool x
let string_value x = Fstring x

type change_type =
  | Unchanged
  | Added
  | Removed
  | Modified
[@@deriving yojson]

type domain_type =
  | DTLiveset
  | DTTrack
  | DTDevice
  | DTClip
  | DTAutomation
  | DTMixer
  | DTRouting
  | DTLocator
  | DTParam
  | DTNote
  | DTEvent
  | DTSend
  | DTPreset
  | DTMacro
  | DTSnapshot
  | DTLoop
  | DTSignature
  | DTSampleRef
  | DTVersion
  | DTOther
[@@deriving yojson, jsonschema]

(* Canonical name/display tables for [domain_type].
   These are the single source of truth for variant↔string mapping; the
   PPX-generated yojson functions, [Config.domain_type_to_string] and the
   ViewSpec builder's [domain_type_of_name] all derive from this enum, so
   adding a variant produces a compile-time exhaustiveness error here
   instead of a silent DTOther / missing-stat fallback. *)
let domain_type_to_name (dt : domain_type) : string =
  match dt with
  | DTLiveset -> "DTLiveset"
  | DTTrack -> "DTTrack"
  | DTDevice -> "DTDevice"
  | DTClip -> "DTClip"
  | DTAutomation -> "DTAutomation"
  | DTMixer -> "DTMixer"
  | DTRouting -> "DTRouting"
  | DTLocator -> "DTLocator"
  | DTParam -> "DTParam"
  | DTNote -> "DTNote"
  | DTEvent -> "DTEvent"
  | DTSend -> "DTSend"
  | DTPreset -> "DTPreset"
  | DTMacro -> "DTMacro"
  | DTSnapshot -> "DTSnapshot"
  | DTLoop -> "DTLoop"
  | DTSignature -> "DTSignature"
  | DTSampleRef -> "DTSampleRef"
  | DTVersion -> "DTVersion"
  | DTOther -> "DTOther"

let domain_type_to_display (dt : domain_type) : string =
  match dt with
  | DTLiveset -> "Liveset"
  | DTTrack -> "Track"
  | DTDevice -> "Device"
  | DTClip -> "Clip"
  | DTAutomation -> "Automation"
  | DTMixer -> "Mixer"
  | DTRouting -> "Routing"
  | DTLocator -> "Locator"
  | DTParam -> "Param"
  | DTNote -> "Note"
  | DTEvent -> "Event"
  | DTSend -> "Send"
  | DTPreset -> "Preset"
  | DTMacro -> "Macro"
  | DTSnapshot -> "Snapshot"
  | DTLoop -> "Loop"
  | DTSignature -> "Signature"
  | DTSampleRef -> "SampleRef"
  | DTVersion -> "Version"
  | DTOther -> "Other"

let domain_type_of_name (name : string) : domain_type =
  match name with
  | "DTLiveset" -> DTLiveset
  | "DTTrack" -> DTTrack
  | "DTDevice" -> DTDevice
  | "DTClip" -> DTClip
  | "DTAutomation" -> DTAutomation
  | "DTMixer" -> DTMixer
  | "DTRouting" -> DTRouting
  | "DTLocator" -> DTLocator
  | "DTParam" -> DTParam
  | "DTNote" -> DTNote
  | "DTEvent" -> DTEvent
  | "DTSend" -> DTSend
  | "DTPreset" -> DTPreset
  | "DTMacro" -> DTMacro
  | "DTSnapshot" -> DTSnapshot
  | "DTLoop" -> DTLoop
  | "DTSignature" -> DTSignature
  | "DTSampleRef" -> DTSampleRef
  | "DTVersion" -> DTVersion
  | "DTOther" -> DTOther
  | _ ->
    (* Unknown names used to silently map to DTOther, masking stale
       mappings when a variant was added. Now raise so the mismatch is
       loud; every shipped caller passes a literal produced by the PPX
       or this table. *)
    invalid_arg
      (Printf.sprintf "Output_types.domain_type_of_name: unknown domain_type %S" name)
