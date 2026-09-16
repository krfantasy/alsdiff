(* Force-link the deriver registrations (see lib/ppx/link.ml). *)
let () = ignore (Ppx_impl.Ppx_patch.deriver, Ppx_impl.Ppx_view_spec.deriver, Ppx_impl.Ppx_id.deriver)

let () = Ppxlib.Driver.standalone ()
