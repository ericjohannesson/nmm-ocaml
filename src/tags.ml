
open Doc_types

let expand_tag_default (tag : Doc_types.ts_tag) : (string * string) option =
match tag with

|Cs_tag "ABBR" -> Some("ABBREVIATION","Abbreviation")
|Cs_tag "ASM" -> Some("ASSUMPTION","Assumption")
|Cs_tag "CONJ" -> Some("CONJECTURE","Conjecture")
|Cs_tag "CONV" -> Some("CONVENTION","Convention")
|Cs_tag "COR" -> Some("COROLLARY","Corollary")
|Cs_tag "DEF" -> Some("DEFINITION","Definition")
|Cs_tag "EX" -> Some("EXAMPLE","Example")
|Cs_tag "FCT" -> Some("FACT","Fact")
|Cs_tag "LMA" -> Some("LEMMA","Lemma")
|Cs_tag "NTN" -> Some("NOTATION","Notation")
|Cs_tag "PRF" -> Some("PROOF","Proof")
|Cs_tag "PRP" -> Some("PROPOSITION","Proposition")
|Cs_tag "QTN" -> Some("QUOTATION","Quotation")
|Cs_tag "RMK" -> Some("REMARK","Remark")
|Cs_tag "THM" -> Some("THEOREM","Theorem")
|Cs_tag "TMY" -> Some("TERMINOLOGY","Terminology")
|Cs_tag "ABBRS" -> Some("ABBBREVIATIONS","Abbbreviations")
|Cs_tag "ASMS" -> Some("ASSUMPTIONS","Assumptions")
|Cs_tag "CONJS" -> Some("CONJECTURES","Conjectures")
|Cs_tag "CONVS" -> Some("CONVENTIONS","Conventions")
|Cs_tag "CORS" -> Some("COROLLARIES","Corollaries")
|Cs_tag "DEFS" -> Some("DEFINITIONS","Definitions")
|Cs_tag "EXS" -> Some("EXAMPLES","Examples")
|Cs_tag "FCTS" -> Some("FACTS","Facts")
|Cs_tag "LMAS" -> Some("LEMMAS","Lemmas")
|Cs_tag "NTNS" -> Some("NOTATIONS","Notations")
|Cs_tag "PRFS" -> Some("PROOFS","Proofs")
|Cs_tag "PRPS" -> Some("PROPOSITIONS","Propositions")
|Cs_tag "QTNS" -> Some("QUOTATIONS","Quotations")
|Cs_tag "RMKS" -> Some("REMARKS","Remarks")
|Cs_tag "THMS" -> Some("THEOREMS","Theorems")
|Cs_tag "PAR"
|Cs_tag "ITM"
|Cs_tag "DSP"
|Cs_tag "BIB" -> None
|Cs_tag s -> let _ : unit = Debug_utils.print_warning ("WARNING: undefined tag: " ^ s) in None




let tag_of_string_opt_default (s : string) : ts_tag option =
	match s with
	|"DEF" -> Some (Cs_tag "DEF")
	|_ -> None

let tag_blk_itm_gen (tag_of_string_opt : string -> ts_tag option) (blk_itm : Doc_types.tr_blk_itm) : Doc_types.tr_blk_itm =
	match blk_itm.fld_blk_itm_tag_or_id with
	|Some _ -> blk_itm
	|None ->
		match blk_itm.fld_blk_itm_main with
		|Cs_blks (blk_list : tu_blk list) ->
			match blk_list with
			|[] -> blk_itm
			|blk_list_hd::blk_list_tl ->
				match blk_list_hd with
				|Cu_blk_txt blk_txt -> (
					match blk_txt with
					|Cs_blk_txt txt_units ->
						match txt_units with
						|Cs_txt_units txt_unit_list ->
							match txt_unit_list with
							|tag_unit::(space_unit::(txt_unit_list_tl)) -> (
								match tag_unit, space_unit with
								|Cu_txt_unit_wysiwyg (Cs_txt_unit_wysiwyg tag_string),
								 Cu_txt_unit_wysiwyg (Cs_txt_unit_wysiwyg " ") -> (
									match tag_of_string_opt tag_string with
									|None -> blk_itm
									|Some tag -> {
										fld_blk_itm_lbl = blk_itm.fld_blk_itm_lbl;
										fld_blk_itm_tag_or_id = Some (Cu_tag_or_id_tag tag);
										fld_blk_itm_main = Cs_blks (
											(Cu_blk_txt (
												Cs_blk_txt (
													Cs_txt_units txt_unit_list_tl)))::blk_list_tl
										)
									}
								)
								|_,_ -> blk_itm
							)
							|tag::[] -> (
								match tag, blk_list_tl with
								|Cu_txt_unit_wysiwyg (Cs_txt_unit_wysiwyg tag_string), _::_ -> (
									match tag_of_string_opt tag_string with
									|None -> blk_itm
									|Some tag -> {
										fld_blk_itm_lbl = blk_itm.fld_blk_itm_lbl;
										fld_blk_itm_tag_or_id = Some (Cu_tag_or_id_tag tag);
										fld_blk_itm_main = Cs_blks blk_list_tl;
									}
								)
								|_,_ -> blk_itm
							)
							|_ -> blk_itm
				)
				|_ -> blk_itm

let tag_blk_itm_default (blk_itm : tr_blk_itm) : tr_blk_itm =
	tag_blk_itm_gen tag_of_string_opt_default blk_itm
