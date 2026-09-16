(* Force-link the deriver registrations from [ppx_impl]: the (pps ...) driver
   links this library's modules, and these references drag the impl archive
   (whose top-level [Deriving.add] calls register patch/id/view_spec) into the
   link. Without them the derivers silently vanish from the driver. *)
let () =
  ignore
    ( Ppx_impl.Ppx_patch.deriver,
      Ppx_impl.Ppx_view_spec.deriver,
      Ppx_impl.Ppx_id.deriver )
