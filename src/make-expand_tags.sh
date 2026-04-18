COL='\([^\t]*\)'
TAB='\t'

SED_CMD_SINGULAR='s/'$COL$TAB$COL$TAB$COL$TAB$COL$TAB$COL$TAB$COL'/|Cs_tag "\1" -> Some("\4","\3")\n/'

SED_CMD_PLURAL='s/'$COL$TAB$COL$TAB$COL$TAB$COL$TAB$COL$TAB$COL'/|Cs_tag "\2" -> Some("\6","\5")\n/'

INTRO='
open Doc_types

let expand_tag (tag : Doc_types.ts_tag) : (string * string) option =
match tag with
'

OUTRO='
|Cs_tag "PAR"
|Cs_tag "ITM"
|Cs_tag "DSP"
|Cs_tag "BIB" -> None
|Cs_tag s -> let _ : unit = Debug_utils.print_warning ("WARNING: undefined tag: " ^ s) in None
'

echo "$INTRO" > expand_tags.ml

sed "$SED_CMD_SINGULAR" ../bin/tags.tsv | grep -v 'Cs_tag ""' >> expand_tags.ml

sed "$SED_CMD_PLURAL" ../bin/tags.tsv | grep -v 'Cs_tag ""' >> expand_tags.ml

echo "$OUTRO" >> expand_tags.ml

