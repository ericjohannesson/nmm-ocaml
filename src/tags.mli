(** Specifies default tags and their expansion *)

val expand_tag_default : Doc_types.ts_tag -> (string * string) option

val tag_of_string_opt_default : string -> Doc_types.ts_tag option

val tag_blk_itm_gen : (string -> Doc_types.ts_tag option) -> Doc_types.tr_blk_itm -> Doc_types.tr_blk_itm

val tag_blk_itm_default : Doc_types.tr_blk_itm -> Doc_types.tr_blk_itm
