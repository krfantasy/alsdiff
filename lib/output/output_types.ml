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

(* Canonical name/display tables for [domain_type]. Adding a variant forces a
   compile-time exhaustiveness error in all three matches below, so a stale
   mapping can no longer silently fall back to DTOther or skip a stat.

   [domain_type_to_name] / [domain_type_of_name] are the canonical variant↔name
   pair. [to_name]'s only consumer today is the [test_domain_type_mapping]
   round-trip, but it is the designated single source for the DT-prefixed name
   should serialization or keying need it (rather than each caller hard-coding
   the string).

   Caveat: the [@@deriving yojson] pair generated on the type above is a
   *separate*, independently generated source — it only coincidentally emits
   the same DT-prefixed strings as [domain_type_to_name]; adding a variant does
   not mechanically sync it, and [test_domain_type_mapping] guards that
   alignment. [Config.domain_type_to_string] is likewise its own function,
   asserted equal to [domain_type_to_display] by the same test rather than
   derived from it. *)
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
    (* Unknown names used to silently map to DTOther, masking stale mappings
       when a variant was added; now we raise so the mismatch is loud.

       Trust contract: [domain_type_of_name] parses *canonical* literals only
       — the DT-prefixed strings emitted by the PPX ([B.domain_type_of_name
       "DT..."]) or by [domain_type_to_name] above. It must NOT be fed
       untrusted input (CLI flags, JSON config, strings parsed from a [.als]);
       the typed [domain_type] value is the trust boundary, and any external
       string must be validated before reaching here. The raise is intentional
       and is guarded by [test_of_name_rejects_unknown]. *)
    invalid_arg
      (Printf.sprintf "Output_types.domain_type_of_name: unknown domain_type %S" name)

(* The complete set of [domain_type] variants. Single source consumed by
   [Stats_renderer.stats_from_config] and the [test_domain_type_mapping]
   round-trip tests, so a new variant only has to be registered once. The order
   is the stats display priority (it pins the line order of stats output, which
   baselines depend on), not the type-declaration order above; DTNote is hoisted
   next to DTClip so musical content groups first. Adding a variant to the type
   still requires registering it here and in the name/display/of_name tables —
   those tables are match-checked (compile error if missed), but this list is
   not, so it is the one manual step the compiler cannot enforce. *)
let all_domain_types : domain_type list =
  [
    DTLiveset; DTTrack; DTDevice; DTClip; DTNote;
    DTAutomation; DTMixer; DTRouting; DTLocator;
    DTParam; DTEvent; DTSend; DTPreset; DTMacro;
    DTSnapshot; DTLoop; DTSignature; DTSampleRef;
    DTVersion; DTOther;
  ]
