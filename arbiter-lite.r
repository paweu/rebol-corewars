

REBOL [
	Name: "Hegemon's Arbiter"
	Date: 19-08-2002
	Description: "MARS"
	Author: "Pawe³ Gawroñski"
	charset: "CP1250"
	Version: 0.1
]

prog-name: "Hegemon's Arbiter"

random/seed now/precise

instruction: make object! [
	Opcode: "DAT"
	Modifier: "F"
	AMode: "$"
	ANumber: 0
	BMode: "$"
	BNumber: 0
	comment: ""
	no: 0
]

warrior: make object! [
	origin:	0
	filename: none
	name: ""
	code: []
]

do %printf.r

error: off
error2: ""


;;;
;;; Core runtime variables (default)
;;;

Settings: make object! [

Core-Size:			8192
Cycles-Before-Tie:		100000
Initial-Instruction:            "DAT.F $0, $0"
Instruction-Limit:              300
Maximum-Number-of-Tasks:        64
Minimum-Separation:             300
Read-Distance:                  8192
Separation:                     "RANDOM"
Warriors:                       2
Write-Distance:                 8192
Core-Display:			true
Core-Disassemble:		true
Skin:					%aqua-blue.r

]

if exists? %settings.txt [
	settings: do load %settings.txt
]

	skins: stylize [
		backdrop-norm: backdrop
	]
	
    
;;;
;;; funkcja kompiluj¹ca, 
;;; file (file!) - nazwa pliku .red

compile: func [ file ][

		; wyzerowanie zmiennych
	lines: read/lines file
	rawcode: copy []
	code: copy []
	labels: copy []
	constants: copy []
	constants1: copy []
	loadfile: copy ""
	counter: 0
	origin: none
	wname: copy ""

		; makra do parsowania
	letter: charset [#"a" - #"z" #"A" - #"Z" #"_"]
    	digit: charset [#"0" - #"9"]
	amode: charset [#"#" #"$" #"@" #"<" #">" #"*" #"{" #"}" ]
    	letter-or-digit: union letter digit
    	valid-name: [letter any letter-or-digit]
	ws:  charset {^A^B^C^D^E^F^G^H^-^/^K^L^M^N^O^P^Q^R^S^T^U^V^W^X^Y^Z^[^\^]^!^_ ^~ }
	instrukcja: [ "DAT" | "MOV" | "ADD" | "SUB" | "MUL" | "DIV" | "MOD" | "JMP" |
                 "JMZ" | "JMN" | "DJN" | "CMP" | "SLT" | "SPL" | 
		 "END" | "ORG" | "EQU" | "FOR" | "ROF" | "=" |
		 "SEQ" | "SNE" | "NOP "] ;pMARS extension
	redcode: [ "DAT" | "MOV" | "ADD" | "SUB" | "MUL" | "DIV" | "MOD" | "JMP" |
                 "JMZ" | "JMN" | "DJN" | "CMP" | "SLT" | "SPL" |
		 "SEQ" | "SNE" | "NOP "] ;pMARS extension

	modyfikator: [ "F" | "I" | "AB" | "BA" | "A" | "B" | "X" ]

	;;;
	;;; PASS 1
	;;;

	i: 0
	line-cache: copy []

		; dla ka¿dej linii kodu...
	while [not all [tail? lines empty? line-cache]] [
		
	either empty? line-cache [
		i: i + 1
		line: first lines
		lines: next lines
	][
		line: first line-cache
		remove line-cache 
	]


		; zerowanie zmiennych
	Label: copy ""		; etykieta
	Opcode: copy ""		; instrukcja
	Modifier: copy ""	; modyfikator
	mode1: copy ""		; tryb A
	num1: copy ""		; adres A
	mode2: copy ""		; tryb B
	num2: copy ""		; adres B
	comment: copy ""	; komentarz

		; parsowanie pojedynczej linii
	if not parse/all line [ 
			any ws		; instrukcja, etykieta, lub obie, ew. ¿adna
			[
				copy Opcode instrukcja opt [ "." copy Modifier 1 2 letter ] [ ws | end ]
				| copy Label valid-name [ ws | end | ":" ]
				any ws 
				copy Opcode instrukcja opt [ "." copy Modifier 1 2 letter ] [ ws | end ]
				| copy Label valid-name [ ws | end | ":" ] (Opcode: "")
				| any ws (Label: Opcode: "")
			]
			any ws		; argument A
			opt [ opt [ copy mode1 amode ]
				any ws copy num1 [to "," | to "#" | to "$" | to "@" | to "<" | to ">" | 
				to "{" | to "}" | to ";" | to end ] 
			]
			any ws		; argument B
			opt [ opt [","] opt [ any ws copy mode2 amode ]
				any ws copy num2 [to ";" | to end]
			]
			any ws		; komentarz
			opt [ to ";" copy Comment to end]
			to end
		]
	[ 
		error: rejoin ["Parse error in line " i]
		error2: line
		break
	]
	
		; tryby domyœlne
	if mode1 = "" [mode1: "$"]
	if mode2 = "" [mode2: "$"]
		; je¿eli by³ tylko jeden argument, to jest argument drugi...
	isAnum: on
	isBnum: on
	if any [none? num1 "" = trim num1][ num1: "0" isAnum: off ]
	if any [none? num2 "" = trim num2][ num2: "0" isBnum: off ]
	if all [not isBnum opcode = "DAT" ][
		mode2: mode1
		num2: num1
		mode1: "$"
		num1: "0"
	]
	if all [isAnum "" = opcode][
		;error: rejoin ["Parse error in line " i]
		;error2: line
		;break
		comment: num1
		num1: "0"
		isAnum: false
	]



		; domyœlne rozszerzenia ICWS'88 -> ICWS'94
	if not parse modifier MODYFIKATOR [
		if parse opcode [ "DAT" | "NOP" ][
			modifier: "F"
		]
		if parse opcode [ "MOV" | "CMP" | "SEQ" | "SNE" ][
			modifier: "I"
			if mode1 = "#" [ modifier: "AB" ]
			if all [not mode1 = "#" mode2 = "#"][ modifier: "B" ]		
		]
		if parse opcode [ "ADD" | "SUB" | "MUL" | "DIV" | "MOD" ][
			modifier: "F"
			if mode1 = "#" [ modifier: "AB" ]
			if all [not mode1 = "#" mode2 = "#"][ modifier: "B" ]
		]
		if parse opcode [ "SLT" ][
			modifier: "B"
			if mode1 = "#" [ modifier: "AB" ]
		]
		if parse opcode [ "JMP" | "JMZ" | "JMN" | "DJN" | "SPL" ][
			modifier: "B"
		]
	]

		; sprawdzanie b³êdów


		; czy instrukcje prawid³owe?
	if not opcode = "" [ 
	if not parse opcode INSTRUKCJA [
		error: rejoin [{Unknown operation "} opcode {" in line } i]
		error2: line
		break
	]]

		; przypisywanie wartoœci etykietom i sta³ym
	switch/default Opcode [
	"equ" [
		if isBnum [ num1: rejoin [num1 " , " mode2 num2]]
		either not label = "" [
			was: false foreach [l v] constants1 [if l = label [was: true]]
			either not was [
				append constants1 label 
				append constants1 num1
			][
				error: rejoin ["Label '" label "' declared twice: line " i]
				error2: line
				break
			]
		][ 
			either empty? constants1 [
				error: rejoin ["Constant name or value missing in line " i]
				error2: line
				break
			][
				append last constants1 join newline num1 
			]
		]
	]
	"=" [
		if isBnum [ num1: rejoin [num1 " , " mode2 num2]]
		either not label = "" [
			was: false foreach [l v] constants1 [if l = label [was: true]]
			either not was [
				append constants1 label 
				append constants1 num1
			][
				error: rejoin ["Label '" label "' declared twice: line " i]
				error2: line
				break
			]
		][ 
			either empty? constants1 [
				error: rejoin ["Constant name or value missing in line " i]
				error2: line
				break
			][
				append last constants1 join newline num1 
			]
		]
	]
	"for" [
		replace/all/case num1 "+" " + "
		replace/all/case num1 "-" " - "
		replace/all/case num1 "*" " * "
		replace/all/case num1 "/" " / "
		replace/all/case num1 "(" " ( "
		replace/all/case num1 ")" " ) "
		replace/all/case num1 "%" " // "
		foreach [lab val] constants1 [
			num1: replace/all/case num1 rejoin [" " lab " "] rejoin [" " val " "]
		]

		if error? try [do num1][
			error: rejoin ["Number of iterations has to be explicit in line: " i]
			error2: line
			break
		]
		for-cnt: 1
		for-i: 1
		for-cache: copy []
		while [not all [tail? lines empty? line-cache]][
			either empty? line-cache [
				i: i + 1
				line: first lines
				lines: next lines
			][
				line: first line-cache
				remove line-cache 
			]
			
			if parse/all line [any ws "rof" any ws to end][
				for-cnt: for-cnt - 1
				if for-cnt = 0 [break]
			]
			if parse/all line [any ws opt valid-name any ws "for" ws to end][
				for-cnt: for-cnt + 1
			]
				replace/all/case line "+" " + "
				replace/all/case line "-" " - "
				replace/all/case line "*" " * "
				replace/all/case line "/" " / "
				replace/all/case line "(" " ( "
				replace/all/case line ")" " ) "
				replace/all/case line "%" " // "
				replace/all/case line "#" " # "
				replace/all/case line "$" " $ "
				replace/all/case line "@" " @ "
				replace/all/case line "<" " < "
				replace/all/case line ">" " > "
				replace/all/case line "{" " { "
				replace/all/case line "}" " } "


				append for-cache rejoin [" " line " "] 
			
		]
		for-temp: copy []
		loop to-integer do num1 [
			foreach line for-cache [
				for-r: to-string for-i
				if 1 = length? for-r [insert for-r "0"]
				temp: copy line
				replace/all/case temp rejoin [" " label " "] to-string for-r
				replace/all/case temp rejoin ["&" label] to-string for-r
				append for-temp temp
			]
			for-i: for-i + 1
		]
		insert line-cache for-temp

	]]

	[		
		if not Label = "" [
			was: false foreach [l v] constants1 [if l = label [was: true]]
			either was [
				either opcode = "" [
					insert line-cache parse/all select constants1 label "^/"
				][
					error: rejoin ["Label '" label "' declared twice: line " i]
					error2: line
					break
				]
			][
				was2: false foreach [l v] labels [if l = label [was2: true]]
				if was2 [
					error: rejoin ["Label '" label "' declared twice: line " i]
					error2: line
					break
				]
				append labels label
				append labels counter
			]
		]
	]


		; wyszukiwanie pocz¹tku kodu
	if parse opcode [ "ORG" ][
		either isAnum [
			origin: num1
		][
			origin: num1: to-string counter
		]
	]
		; nie kompiluj po END
	if parse opcode [ "END" ][
		if isAnum [ origin: num1 ]
		break
	]

	opc: opcode
	mod: modifier
	com: comment
	m: make instruction [
		opcode: uppercase opc
		modifier: uppercase mod
		amode: mode1
		anumber: num1
		bmode: mode2
		bnumber: num2
		comment: com
		no: i
	]
	if parse opcode REDCODE [
		if none? origin [ origin: form counter ]
		counter: counter + 1	
	]
	append rawcode m
	;print reform [opcode mode1 num1 mode2 num2]

	
		
	]

	if error [
		exit
	]

	;;;
	;;; PASS 2
	;;;
	lines: head lines

	counter: 0

	if none? origin [ origin: "start" ]

	num: rejoin [" " copy origin " "]
	replace/all/case num "+" " + "
	replace/all/case num "-" " - "
	replace/all/case num "*" " * "
	replace/all/case num "/" " / "
	replace/all/case num "(" " ( "
	replace/all/case num ")" " ) "
	replace/all/case num "%" " // "
				replace/all/case num "#" " # "
				replace/all/case num "$" " $ "
				replace/all/case num "@" " @ "
				replace/all/case num "<" " < "
				replace/all/case num ">" " > "
				replace/all/case num "{" " { "
				replace/all/case num "}" " } "
	foreach [lab val] labels [
		num: replace/all/case num rejoin [" " lab " "] rejoin [" " val " "]
	]
	foreach [lab val] constants [
		num: replace/all/case num rejoin [" " lab " "] rejoin [" " val " "]
	]
	if error? try [
		num: do num
	][
		error: rejoin ["Cannot evaluate the origin"]
		error2: origin
		exit
	]
	origin: num

	foreach [label num1] constants1 [
		num: rejoin [" " copy num1 " " ]
		replace/all/case num "+" " + "
		replace/all/case num "-" " - "
		replace/all/case num "*" " * "
		replace/all/case num "/" " / "
		replace/all/case num "(" " ( "
		replace/all/case num ")" " ) "
		replace/all/case num "%" " // "
				replace/all/case num "#" " # "
				replace/all/case num "$" " $ "
				replace/all/case num "@" " @ "
				replace/all/case num "<" " < "
				replace/all/case num ">" " > "
				replace/all/case num "{" " { "
				replace/all/case num "}" " } "
		foreach [lab val] constants [
			num: replace/all/case num rejoin [" " lab " "] rejoin [" " val " "]
		]
		append constants label 
		append constants num
	]


	foreach line rawcode [

	if parse line/opcode REDCODE [

		num: copy rejoin [" " line/ANumber " "]
		replace/all/case num "+" " + "
		replace/all/case num "-" " - "
		replace/all/case num "*" " * "
		replace/all/case num "/" " / "
		replace/all/case num "(" " ( "
		replace/all/case num ")" " ) "
		replace/all/case num "%" " // "
				replace/all/case num "#" " # "
				replace/all/case num "$" " $ "
				replace/all/case num "@" " @ "
				replace/all/case num "<" " < "
				replace/all/case num ">" " > "
				replace/all/case num "{" " { "
				replace/all/case num "}" " } "

		foreach [lab val] constants [
			num: replace/all/case num rejoin [" " lab " "] rejoin [" " val " "]
		]
		foreach [lab val] labels [
			num: replace/all/case num rejoin [" " lab " "] rejoin [" " val - counter " "]
		]
		if error? try [
			num: do num
		][
			error: rejoin ["Cannot evaluate the ANumber in line " line/no]
			error2: pick lines line/no
			halt
			exit
		]

		line/ANumber: num
		num: copy rejoin [" " line/BNumber " "]
		replace/all/case num "+" " + "
		replace/all/case num "-" " - "
		replace/all/case num "*" " * "
		replace/all/case num "/" " / "
		replace/all/case num "(" " ( "
		replace/all/case num ")" " ) "
		replace/all/case num "%" " % "
				replace/all/case num "#" " # "
				replace/all/case num "$" " $ "
				replace/all/case num "@" " @ "
				replace/all/case num "<" " < "
				replace/all/case num ">" " > "
				replace/all/case num "{" " { "
				replace/all/case num "}" " } "

		foreach [lab val] constants [
			num: replace/all/case num rejoin [" " lab " "] rejoin [" " val " "]
		]
		foreach [lab val] labels [
			num: replace/all/case num rejoin [" " lab " "] rejoin [" " val - counter " "]
		]
		if error? try [
			num: do num
		][
			error: rejoin ["Cannot evaluate the BNumber in line " line/no]
			error2: pick lines line/no
			halt
			exit
		]
		line/BNumber: num
	]
	if parse line/opcode REDCODE [
		counter: counter + 1
		append code line
	]
	if parse line/opcode [ "EQU" ][
		line/opcode: line/modifier: ""
	]

	Opc: uppercase either not "" = line/modifier 
		[rejoin [line/Opcode "." line/modifier]][ line/opcode ]
	args: copy ""
	if parse line/opcode REDCODE [
		append args rejoin [line/Amode line/ANumber ", " line/Bmode line/BNumber]]
	if parse line/opcode [ "END" | "ORG" ][
		append args rejoin [line/ANumber]]
 	com: to-string line/comment
	either all [
		parse com [ ";" ["redcode"|"name"|"author"|"version"|"date"|"strategy"|"kill"] to end ]
		opc = ""
	][
		append loadfile sprintf "%s\n" [com]
	][
		append loadfile sprintf "    %-7s %-15s %s\n" compose [Opc args com]
	] 
	
	parse com [";" "name" copy wname to end]

	]
	if Settings/Instruction-Limit < length? code [
		error: "Too many instructions"
		error2: ""
	]

	if error [
		exit
	]
	
]


disas-ptr: 1

disas-init: func [][
	PC: 1
	disas: layout [
	styles skins backdrop-norm
	banner "Disassemble:"
	list-up: area 250x80 
	across
	disas-led: box 23x23 ibevel none 4x4
	f-op: field 60 "DAT.F" [
		if parse f-op/text [ 
			copy o1 REDCODE  
			copy o2 MODYFIKATOR ][
			core/:disas-ptr/opcode: uppercase o1
			core/:disas-ptr/modifier: uppercase o2
		]
	]
	f-a: field 60 "#0" [
		if parse f-a/text [
			copy m1 amode
			copy m2 to end ][
			if not error? try [ m2: to-integer m2 ][
				core/:disas-ptr/AMode: m1
				core/:disas-ptr/ANumber: m2
			]
		]
	]
	f-b: field 60 "#0" [
		if parse f-b/text [
			copy m1 amode
			copy m2 to end ][
			if not error? try [ m2: to-integer m2 ][
				core/:disas-ptr/BMode: m1
				core/:disas-ptr/BNumber: m2
			]
		]
	]
	below
	list-down: area 250x80 
	across
	arrow up keycode [ up ] [ disas-do circ disas-ptr - 1 ]
	arrow down keycode [ down ] [ disas-do circ disas-ptr + 1 ]
	]
	view/new/title disas "Disassembled"
	disas-do 1
	
]

disas-do: func [ ptr ][
	disas-ptr: ptr
	list-up/text: copy ""
	a: circ ptr - 5
	loop 5 [
	append list-up/text sprintf "   %10s %10s, %10s\n" reduce [
		rejoin [core/:a/Opcode "." core/:a/Modifier ]
		join core/:a/AMode core/:a/ANumber
		join core/:a/BMode core/:a/BNumber
	]
	a: circ a + 1
	]
	f-op/text: rejoin [core/:ptr/Opcode "." core/:ptr/Modifier]
	f-a/text: join  core/:ptr/AMode core/:ptr/ANumber
	f-b/text: join core/:ptr/BMode core/:ptr/BNumber
	disas-led/color: pick colors ((que-index - 1) // 8) + 1
	list-down/text: copy ""
	a: circ ptr + 1
	loop 5 [
	append list-down/text sprintf "   %10s %10s, %10s\n" reduce [
		rejoin [core/:a/Opcode "." core/:a/Modifier ]
		join core/:a/AMode core/:a/ANumber
		join core/:a/BMode core/:a/BNumber
	]
	a: circ a + 1
	]
	show disas
]

	colors: [
		0.0.255   ;- 1 Blue
		0.240.0   ;- 2 Mid Green
		255.255.0 ;- 3 Yellow
		255.0.0   ;- 8 Red
		0.0.139   ;- 4 Dark Blue
		0.100.0   ;- 5 Dark Green
		100.0.0   ;- 6 Dark Red
		0.255.255 ;- 7 Aqua
	
	]  

	square: make face [
		offset: 0x0
		size: 8x8
		text: none
		;font: make font [style: 'bold size: 14]
		color: 200.200.200
		edge: make edge [
			size: 1x1
			color: 150.150.150
			effect: 'bevel
		]
		effect: none
		square-no: none
		data: off
		right-down: false
		away: false
		feel: make feel [
			redraw: func [face][
			]
			engage: func [face action event /local result][
				if action = 'down [ 
					if Settings/Core-Disassemble [ disas-do face/square-no ]
				]            
			]
		]
	]

update-square: func [ sq ][
	insq: pick core sq/square-no
	sq/color: pick colors ((insq/no - 1) // 8) + 1
	sq/edge/color: pick colors ((insq/no - 1) // 8) + 1

	eff: none
	foreach q que [
		if find q sq/square-no [ eff: [ oval ] ]
	]
	sq/effect: eff
	show sq
]

make-grid: func [M S][
	grid: make face [
		offset: 24x48 
		pane: copy [] 
		size: (8 * to-pair reduce [ columns: M rows: to-integer (S / M) + 1 ]) + 8x8
		edge: make edge [effect: 'ibevel]
		color: 200.200.200
		
	]
	no: 1
	repeat row rows [
		repeat column columns [
			if Settings/Core-Size >= no [
			append grid/pane make square [
				offset: to-pair reduce [ (column - 1) * 8 + 2 (row - 1) * 8 + 2 ]
				square-no: no
			]
			no: no + 1
			]
		]
	]
	foreach sq grid/pane [ update-square sq ]
	return grid
]

display-init: func [][
	M: Settings/Core-Size
	M: square-root M
	M: 4 * M / 3
	M: to-integer M
	grid: make-grid M Settings/Core-Size

	view/new/title grid "Core"
]

core: []
core-init: func [][
	switch Settings/Initial-Instruction [
	"NONE" [
		while [(length? core) < Settings/Core-Size][
			append core make instruction []
		]
	]
	"DAT.F $0, $0" [
		core: copy []
		while [(length? core) < Settings/Core-Size][
			append core make instruction []
		]
	]
	"RANDOM" [
		core: copy []
		while [(length? core) < Settings/Core-Size][
			append core make instruction [
				Opcode: first random [ "DAT" "MOV" "ADD" "SUB" "MUL" "DIV" "MOD"
                 		"JMP" "JMZ" "JMN" "DJN" "CMP" "SLT" "SPL" ]
				Modifier: first random [ "F" "I" "AB" "BA" "A" "B" "X" ]
				AMode: first random ["#" "$" "@" "<" ">"]
				ANumber: (random Settings/Core-Size) - 1
				BMode: first random ["#" "$" "@" "<" ">"]
				BNumber: (random Settings/Core-Size) - 1
			]
		]
	]
	]
]

core-load-warriors: func [ warriors ][
	pos: random Settings/Core-Size
	origins: copy []
	max-interval: to-integer (Settings/Core-Size / (length? Warriors)) - Settings/Minimum-Separation
	foreach w warriors [
		either Settings/Separation = "RANDOM" [
			interval: (random (max-interval - length? w/code)) + Settings/Minimum-Separation
			;print reform ["int" interval length? w/code pos max-interval]
		][
			interval: Settings/Separation
		]
		interval: to-integer interval
		pos: ((pos + interval - 1) // Settings/Core-Size) + 1
		append origins ((pos + w/origin - 1) // Settings/Core-Size) + 1
		foreach c w/code [
			while [ c/ANumber < 0 ][c/ANumber: Settings/Core-Size + c/ANumber]
			c/ANumber:  (c/ANumber // Settings/Core-Size)
			while [ c/BNumber < 0 ][c/BNumber: Settings/Core-Size + c/BNumber]
			c/BNumber:  (c/BNumber // Settings/Core-Size)
			c/no: index? find warriors w
			poke core pos make c []
			;print pos
			pos: (pos // Settings/Core-Size) + 1
		]
	]
	queue-init origins
	;print origins
]


queue-init: func [ origins ][
	que-index: 1
	que: copy []
	que-count: length? origins
	foreach origin origins [ append/only que reduce [ origin ] ]
] 

queue: func [ w pointer ][
	append que/:w pointer
	if Settings/Core-Display [
		update-square grid/pane/:pointer
	]
]

unqueue: func [ w ][
	either 0 < length? que/:w [
		pc: first que/:w
		remove que/:w
		return pc
	][
		return none
	]
]
Fold: func [pointer limit M][
	result: (pointer // limit)
	if result > (limit / 2) [ result: result + M - limit]
	return result
]

Circ: func [ addr ][
	if addr <= 0 [ addr: addr + Settings/Core-Size ]
	return ((addr - 1) // Settings/Core-Size) + 1
]
Circl: func [ addr ][
	;while [addr < 0] [ addr: addr + Settings/Core-Size ]
	return addr // Settings/Core-Size
]

mars: make object! [
	started: false
	do: func [][
		PC: unqueue que-index
	if not none? PC [
		if Settings/Core-Display [
			update-square grid/pane/:PC
		]
		;if Settings/Core-Disassemble [
		;	disas-do PC
		;]
		IR: core/:PC
		either IR/Amode = "#" [
			RPA: WPA: 0
		][
			RPA: Fold IR/ANumber Settings/Read-Distance Settings/Core-Size
			WPA: Fold IR/ANumber Settings/Write-Distance Settings/Core-Size
			if not IR/AMode = "$" [
				a: circ PC + WPA
				b: circ PC + RPA
				switch IR/AMode [
				"<" [
					core/:a/BNumber: Circl core/:a/BNumber + Settings/Core-Size - 1
				RPA: Fold RPA + core/:b/BNumber Settings/Read-Distance Settings/Core-Size
				WPA: Fold WPA + core/:a/BNumber Settings/Write-Distance Settings/Core-Size
				]
				"{" [
					core/:a/ANumber: Circl core/:a/ANumber + Settings/Core-Size - 1
				RPA: Fold RPA + core/:b/ANumber Settings/Read-Distance Settings/Core-Size
				WPA: Fold WPA + core/:a/ANumber Settings/Write-Distance Settings/Core-Size
				]
				">" [
					PIP: circ PC + WPA
				RPA: Fold RPA + core/:b/BNumber Settings/Read-Distance Settings/Core-Size
				WPA: Fold WPA + core/:a/BNumber Settings/Write-Distance Settings/Core-Size
				]
				"}" [
					PIP: circ PC + WPA
				RPA: Fold RPA + core/:b/ANumber Settings/Read-Distance Settings/Core-Size
				WPA: Fold WPA + core/:a/ANumber Settings/Write-Distance Settings/Core-Size
				]
				"@" [
				RPA: Fold RPA + core/:b/BNumber Settings/Read-Distance Settings/Core-Size
				WPA: Fold WPA + core/:a/BNumber Settings/Write-Distance Settings/Core-Size
				]
				"*" [
				RPA: Fold RPA + core/:b/ANumber Settings/Read-Distance Settings/Core-Size
				WPA: Fold WPA + core/:a/ANumber Settings/Write-Distance Settings/Core-Size
				]
				] ;IR/AMode
			]
		]

		b: circ PC + RPA
		IRA: make core/:b []
		if IR/AMode = ">" [ core/:PIP/BNumber: circl core/:PIP/BNumber + 1 ]
		if IR/AMode = "}" [ core/:PIP/ANumber: circl core/:PIP/ANumber + 1 ]

		
		either IR/Bmode = "#" [
			RPB: WPB: 0
		][
			RPB: Fold IR/BNumber Settings/Read-Distance Settings/Core-Size
			WPB: Fold IR/BNumber Settings/Write-Distance Settings/Core-Size
			if not IR/BMode = "$" [
				a: circ PC + WPB
				b: circ PC + RPB
				switch IR/BMode [
				"<" [
					core/:a/BNumber: Circl core/:a/BNumber + Settings/Core-Size - 1
				RPB: Fold RPB + core/:b/BNumber Settings/Read-Distance Settings/Core-Size
				WPB: Fold WPB + core/:a/BNumber Settings/Write-Distance Settings/Core-Size
				]
				"{" [
					core/:a/ANumber: Circl core/:a/ANumber + Settings/Core-Size - 1
				RPB: Fold RPB + core/:b/ANumber Settings/Read-Distance Settings/Core-Size
				WPB: Fold WPB + core/:a/ANumber Settings/Write-Distance Settings/Core-Size
				]
				">" [
					PIP: circ PC + WPB
				RPB: Fold RPB + core/:b/BNumber Settings/Read-Distance Settings/Core-Size
				WPB: Fold WPB + core/:a/BNumber Settings/Write-Distance Settings/Core-Size
				]
				"}" [
					PIP: circ PC + WPB
				RPB: Fold RPB + core/:b/ANumber Settings/Read-Distance Settings/Core-Size
				WPB: Fold WPB + core/:a/ANumber Settings/Write-Distance Settings/Core-Size
				]
				"@" [
				RPB: Fold RPB + core/:b/BNumber Settings/Read-Distance Settings/Core-Size
				WPB: Fold WPB + core/:a/BNumber Settings/Write-Distance Settings/Core-Size
				]
				"*" [
				RPB: Fold RPB + core/:b/ANumber Settings/Read-Distance Settings/Core-Size
				WPB: Fold WPB + core/:a/ANumber Settings/Write-Distance Settings/Core-Size
				]
				] ;IR/BMode
			]
		]
		b: circ PC + RPB
		IRB: make core/:b []
		if IR/BMode = ">" [ core/:PIP/BNumber: circl core/:PIP/BNumber + 1 ]
		if IR/BMode = "}" [ core/:PIP/ANumber: circl core/:PIP/ANumber + 1 ]

		
		;print reform [PC IR/opcode IR/modifier RPA RPB b]
		a: none
		switch IR/Opcode [
		"DAT" [
			que-c: length? que/:que-index
			info/:que-index/text: que-c  ;informacja o liczbie procesów
			show info/:que-index
			if que-c = 0 [ que-count: que-count - 1 ]
		]
		"NOP" [
			queue que-index circ PC + 1
		]
		"MOV" [
			a: circ PC + WPB
			switch IR/Modifier [
				"A" [ core/:a/ANumber: IRA/ANumber ]
				"B" [ core/:a/BNumber: IRA/BNumber ]
				"AB" [ core/:a/BNumber: IRA/ANumber ]
				"BA" [ core/:a/ANumber: IRA/BNumaber ]
				"F" [
					core/:a/ANumber: IRA/ANumber
					core/:a/BNumber: IRA/BNumber
				]
				"X" [
					core/:a/ANumber: IRA/BNumber
					core/:a/BNumber: IRA/ANumber
				]
				"I" [ poke core a make IRA [] ]
			]
			queue que-index circ PC + 1
			if Settings/Core-Display [
				update-square grid/pane/:a
			]
		]
		"ADD" [
			a: circ PC + WPB
			switch IR/Modifier [
			"A" [ core/:a/ANumber: circl IRB/ANumber + IRA/ANumber ]
			"B" [ core/:a/BNumber: circl IRB/BNumber + IRA/BNumber ]
			"AB" [ core/:a/BNumber: circl IRB/BNumber + IRA/ANumber ]	;zmiana
			"BA" [ core/:a/ANumber: circl IRB/ANumber + IRA/BNumber ]	;zmiana
			"F" [ core/:a/ANumber: circl IRB/ANumber + IRA/ANumber 
				core/:a/BNumber: circl IRB/BNumber + IRA/BNumber ]
			"I" [ core/:a/ANumber: circl IRB/ANumber + IRA/ANumber 
				core/:a/BNumber: circl IRB/BNumber + IRA/BNumber ]
			"X" [ core/:a/BNumber: circl IRB/BNumber + IRA/ANumber 	;zmiana
				core/:a/ANumber: circl IRB/ANumber + IRA/BNumber ]
			]
			queue que-index circ PC + 1
			if Settings/Core-Display [
				update-square grid/pane/:a
			]
				
		]
		"SUB" [
			a: circ PC + WPB
			switch IR/Modifier [
			"A" [ core/:a/ANumber: circl IRB/ANumber + Settings/Core-Size - IRA/ANumber ]
			"B" [ core/:a/BNumber: circl IRB/BNumber + Settings/Core-Size - IRA/BNumber ]
			"AB" [ core/:a/BNumber: circl IRB/BNumber + Settings/Core-Size - IRA/ANumber ]	;zmiana
			"BA" [ core/:a/ANumber: circl IRB/ANumber + Settings/Core-Size - IRA/BNumber ]	;zmiana
			"F" [ core/:a/ANumber: circl IRB/ANumber + Settings/Core-Size - IRA/ANumber 
				core/:a/BNumber: circl IRB/BNumber + Settings/Core-Size - IRA/BNumber ]
			"I" [ core/:a/ANumber: circl IRB/ANumber + Settings/Core-Size - IRA/ANumber 
				core/:a/BNumber: circl IRB/BNumber + Settings/Core-Size - IRA/BNumber ]
			"X" [ core/:a/BNumber: circl IRB/BNumber + Settings/Core-Size - IRA/ANumber 	;zmiana
				core/:a/ANumber: circl IRB/ANumber + Settings/Core-Size - IRA/BNumber ]
			]
			queue que-index circ PC + 1
			if Settings/Core-Display [
				update-square grid/pane/:a
			]
				
		]
		"MUL" [
			a: circ PC + WPB
			switch IR/Modifier [
			"A" [ core/:a/ANumber: circl IRB/ANumber * IRA/ANumber ]
			"B" [ core/:a/BNumber: circl IRB/BNumber * IRA/BNumber ]
			"AB" [ core/:a/BNumber: circl IRB/BNumber * IRA/ANumber ]	;zmiana
			"BA" [ core/:a/ANumber: circl IRB/ANumber * IRA/BNumber ]	;zmiana
			"F" [ core/:a/ANumber: circl IRB/ANumber * IRA/ANumber 
				core/:a/BNumber: circl IRB/BNumber * IRA/BNumber ]
			"I" [ core/:a/ANumber: circl IRB/ANumber * IRA/ANumber 
				core/:a/BNumber: circl IRB/BNumber * IRA/BNumber ]
			"X" [ core/:a/BNumber: circl IRB/BNumber * IRA/ANumber 	;zmiana
				core/:a/ANumber: circl IRB/ANumber * IRA/BNumber ]
			]
			queue que-index circ PC + 1
			if Settings/Core-Display [
				update-square grid/pane/:a
			]
				
		]
		"DIV" [
			a: circ PC + WPB
			noqueue: off
			switch IR/Modifier [
			"A" [ either IRA/ANumber = 0 [noqueue: on][core/:a/ANumber: circl to-integer IRB/ANumber / IRA/ANumber] ]
			"B" [ either IRA/BNumber = 0 [noqueue: on][core/:a/BNumber: circl to-integer IRB/BNumber / IRA/BNumber] ]
			"AB" [ either IRA/ANumber = 0 [noqueue: on][core/:a/BNumber: circl to-integer IRB/BNumber / IRA/ANumber] ]
			"BA" [ either IRA/BNumber = 0 [noqueue: on][core/:a/ANumber: circl to-integer IRB/ANumber / IRA/BNumber] ]
			"F" [ 	either IRA/ANumber = 0 [noqueue: on][
				core/:a/ANumber: circl to-integer IRB/ANumber / IRA/ANumber ]
				either IRA/BNumber = 0 [noqueue: on][
				core/:a/BNumber: circl to-integer IRB/BNumber / IRA/BNumber ]]
			"I" [ 	either IRA/ANumber = 0 [noqueue: on][
				core/:a/ANumber: circl to-integer IRB/ANumber / IRA/ANumber ]
				either IRA/BNumber = 0 [noqueue: on][
				core/:a/BNumber: circl to-integer IRB/BNumber / IRA/BNumber ]]
			"X" [ 	either IRA/ANumber = 0 [noqueue: on][
				core/:a/BNumber: circl to-integer IRB/BNumber / IRA/ANumber ]
				either IRA/BNumber = 0 [noqueue: on][
				core/:a/ANumber: circl to-integer IRB/ANumber / IRA/BNumber ]]
			]
			if not noqueue [queue que-index circ PC + 1]
			if Settings/Core-Display [
				update-square grid/pane/:a
			]
		]
		"MOD" [
			a: circ PC + WPB
			noqueue: off
			switch IR/Modifier [
			"A" [ either IRA/ANumber = 0 [noqueue: on][core/:a/ANumber: circl IRB/ANumber // IRA/ANumber] ]
			"B" [ either IRA/BNumber = 0 [noqueue: on][core/:a/BNumber: circl IRB/BNumber // IRA/BNumber] ]
			"AB" [ either IRA/ANumber = 0 [noqueue: on][core/:a/BNumber: circl IRB/BNumber // IRA/ANumber] ]
			"BA" [ either IRA/BNumber = 0 [noqueue: on][core/:a/ANumber: circl IRB/ANumber // IRA/BNumber] ]
			"F" [ 	either IRA/ANumber = 0 [noqueue: on][
				core/:a/ANumber: circl IRB/ANumber // IRA/ANumber ]
				either IRA/BNumber = 0 [noqueue: on][
				core/:a/BNumber: circl IRB/BNumber // IRA/BNumber ]]
			"I" [ 	either IRA/ANumber = 0 [noqueue: on][
				core/:a/ANumber: circl IRB/ANumber // IRA/ANumber ]
				either IRA/BNumber = 0 [noqueue: on][
				core/:a/BNumber: circl IRB/BNumber // IRA/BNumber ]]
			"X" [ 	either IRA/ANumber = 0 [noqueue: on][
				core/:a/BNumber: circl IRB/BNumber // IRA/ANumber ]
				either IRA/BNumber = 0 [noqueue: on][
				core/:a/ANumber: circl IRB/ANumber // IRA/BNumber ]]
			]
			queue que-index circ PC + 1
			if Settings/Core-Display [
				update-square grid/pane/:a
			]
				
		]
		"JMP" [ queue que-index circ PC + RPA ]
		"JMZ" [ 
			switch IR/Modifier [
			"A" [ either IRB/ANumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"BA" [ either IRB/ANumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"B" [ either IRB/BNumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"AB" [ either IRB/BNumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"F" [ either all [IRB/ANumber = 0 IRB/BNumber = 0][ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"X" [ either all [IRB/ANumber = 0 IRB/BNumber = 0][ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"I" [ either all [IRB/ANumber = 0 IRB/BNumber = 0][ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			]
		]
		"JMN" [ 
			switch IR/Modifier [
			"A" [ either not IRB/ANumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"BA" [ either not IRB/ANumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"B" [ either not IRB/BNumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"AB" [ either not IRB/BNumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"F" [ either all [not IRB/ANumber = 0 not IRB/BNumber = 0][ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"X" [ either all [not IRB/ANumber = 0 not IRB/BNumber = 0][ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"I" [ either all [not IRB/ANumber = 0 not IRB/BNumber = 0][ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			]
		]
		"DJN" [ 
			a: circ PC + WPB
			switch IR/Modifier [
			"A" [ core/:a/ANumber: circl core/:a/ANumber + Settings/Core-Size - 1 IRB/ANumber: IRB/ANumber - 1
				either not IRB/ANumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"BA" [ core/:a/ANumber: circl core/:a/ANumber + Settings/Core-Size - 1 IRB/ANumber: IRB/ANumber - 1
				either not IRB/ANumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"B" [ core/:a/BNumber: circl core/:a/BNumber + Settings/Core-Size - 1 IRB/BNumber: IRB/BNumber - 1
				either not IRB/BNumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"AB" [ core/:a/BNumber: circl core/:a/BNumber + Settings/Core-Size - 1 IRB/BNumber: IRB/BNumber - 1
				either not IRB/BNumber = 0 [ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"F" [ 	core/:a/ANumber: circl core/:a/ANumber + Settings/Core-Size - 1 IRB/ANumber: IRB/ANumber - 1
				core/:a/BNumber: circl core/:a/BNumber + Settings/Core-Size - 1 IRB/BNumber: IRB/BNumber - 1
				either all [not IRB/ANumber = 0 not IRB/BNumber = 0][ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"X" [ 	core/:a/ANumber: circl core/:a/ANumber + Settings/Core-Size - 1 IRB/ANumber: IRB/ANumber - 1
				core/:a/BNumber: circl core/:a/BNumber + Settings/Core-Size - 1 IRB/BNumber: IRB/BNumber - 1
				either all [not IRB/ANumber = 0 not IRB/BNumber = 0][ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			"I" [ 	core/:a/ANumber: circl core/:a/ANumber + Settings/Core-Size - 1 IRB/ANumber: IRB/ANumber - 1
				core/:a/BNumber: circl core/:a/BNumber + Settings/Core-Size - 1 IRB/BNumber: IRB/BNumber - 1
				either all [not IRB/ANumber = 0 not IRB/BNumber = 0][ queue que-index circ PC + RPA][queue que-index circ PC + 1]]
			]
		]
		"CMP" [
			switch IR/Modifier [
			"A" [ either IRA/ANumber = IRB/ANumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"B" [ either IRA/BNumber = IRB/BNumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"AB" [ either IRA/ANumber = IRB/BNumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"BA" [ either IRA/BNumber = IRB/ANumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"F" [ either all [IRA/ANumber = IRB/ANumber IRA/BNumber = IRB/BNumber ]
				 [queue que-index circ PC + 2][queue que-index circ PC + 1] ]	
			"X" [ either all [IRA/BNumber = IRB/ANumber IRA/ANumber = IRB/BNumber ]
				 [queue que-index circ PC + 2][queue que-index circ PC + 1] ]	
			"I" [ either all [
				IRA/Opcode = IRB/Opcode
				IRA/Modifier = IRB/Modifier
				IRA/AMode = IRB/AMode
				IRA/ANumber = IRB/ANumber
				IRA/BMode = IRB/BMode
				IRA/BNumber = IRB/BNumber
				][queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			]
		]

		"SEQ" [ ;pMARS extension
			switch IR/Modifier [
			"A" [ either IRA/ANumber = IRB/ANumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"B" [ either IRA/BNumber = IRB/BNumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"AB" [ either IRA/ANumber = IRB/BNumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"BA" [ either IRA/BNumber = IRB/ANumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"F" [ either all [IRA/ANumber = IRB/ANumber IRA/BNumber = IRB/BNumber ]
				 [queue que-index circ PC + 2][queue que-index circ PC + 1] ]	
			"X" [ either all [IRA/BNumber = IRB/ANumber IRA/ANumber = IRB/BNumber ]
				 [queue que-index circ PC + 2][queue que-index circ PC + 1] ]	
			"I" [ either all [
				IRA/Opcode = IRB/Opcode
				IRA/Modifier = IRB/Modifier
				IRA/AMode = IRB/AMode
				IRA/ANumber = IRB/ANumber
				IRA/BMode = IRB/BMode
				IRA/BNumber = IRB/BNumber
				][queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			]
		]
		"SNE" [	;pMARS extension
			switch IR/Modifier [
			"A" [ either IRA/ANumber = IRB/ANumber [queue que-index circ PC + 1][queue que-index circ PC + 2] ]
			"B" [ either IRA/BNumber = IRB/BNumber [queue que-index circ PC + 1][queue que-index circ PC + 2] ]
			"AB" [ either IRA/ANumber = IRB/BNumber [queue que-index circ PC + 1][queue que-index circ PC + 2] ]
			"BA" [ either IRA/BNumber = IRB/ANumber [queue que-index circ PC + 1][queue que-index circ PC + 2] ]
			"F" [ either all [IRA/ANumber = IRB/ANumber IRA/BNumber = IRB/BNumber ]
				 [queue que-index circ PC + 1][queue que-index circ PC + 2] ]	
			"X" [ either all [IRA/BNumber = IRB/ANumber IRA/ANumber = IRB/BNumber ]
				 [queue que-index circ PC + 1][queue que-index circ PC + 2] ]	
			"I" [ either all [
				IRA/Opcode = IRB/Opcode
				IRA/Modifier = IRB/Modifier
				IRA/AMode = IRB/AMode
				IRA/ANumber = IRB/ANumber
				IRA/BMode = IRB/BMode
				IRA/BNumber = IRB/BNumber
				][queue que-index circ PC + 1][queue que-index circ PC + 2] ]
			]
		]


		"SLT" [
			switch IR/Modifier [
			"A" [ either IRA/ANumber < IRB/ANumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"B" [ either IRA/BNumber < IRB/BNumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"AB" [ either IRA/ANumber < IRB/BNumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"BA" [ either IRA/BNumber < IRB/ANumber [queue que-index circ PC + 2][queue que-index circ PC + 1] ]
			"F" [ either all [IRA/ANumber < IRB/ANumber IRA/BNumber < IRB/BNumber ]
				 [queue que-index circ PC + 2][queue que-index circ PC + 1] ]	
			"I" [ either all [IRA/ANumber < IRB/ANumber IRA/BNumber < IRB/BNumber ]
				 [queue que-index circ PC + 2][queue que-index circ PC + 1] ]	
			"X" [ either all [IRA/BNumber < IRB/ANumber IRA/ANumber < IRB/BNumber ]
				 [queue que-index circ PC + 2][queue que-index circ PC + 1] ]	
			]
		]
		"SPL" [
			queue que-index circ PC + 1
			if Settings/Maximum-Number-of-Tasks > length? que/:que-index [queue que-index circ PC + RPA]
			que-c: length? que/:que-index
			info/:que-index/text: que-c  ;informacja o liczbie procesów
			show info/:que-index
		]
		

		]
		if not none? a [
			core/:a/no: que-index
			if Settings/Core-Display [
				update-square grid/pane/:a
			]
		]

	]
		que-index: (que-index // length? que) + 1
		if Settings/Core-Disassemble [
			if not empty? que/:que-index [disas-do first que/:que-index]
		]

	]
	

			
]

cycle-do: func [][
					if all [que-count <= 1 1 < length? warriors ] [
						i: 1
						foreach q que [
							if 0 < length? q [ winner: warriors/:i/Name ]
							i: i + 1
						]
						mars/started: false
						close-windows: on
						que-count: 0
						inform layout [
							styles skins backdrop-norm
							label "The winner is:"
							vh2 winner
							button "OK" 
						]
					]
					if tick = Settings/Cycles-Before-Tie [
						mars/started: false
						close-windows: on
						que-count: 0
						inform layout [
							styles skins backdrop-norm
							label "No winner this time"
							button "OK" 
						]
					]
					tick: tick + 1
					if que-count > 0 [ mars/do ]
]

replace-wlist: does [
	layout [
		styles skins backdrop-norm
		banner "Battle Setup"
		label "Selected Warriors:"
		across 
		rwlist: text-list 315x200 data wl [ wvalue: value ]
	]
	rwlist/parent-face: bat1
	bat1/pane/4: rwlist
	show bat1
	wlist: rwlist
]

battle: func [][
	unview/only main
	seed: now/precise
	wvalue: false
	wl: []
	view/new/title bat1: layout [
		styles skins backdrop-norm
		banner "Battle Setup"
		label "Selected Warriors:"
		across 
		wlist: text-list 315x200 data wl [ wvalue: value ]
		return
		button "Add" #"a" [ 
			if not none? files: request-file/keep/filter ["*.red"] [
			foreach file files [
				append wl file
			]
			replace-wlist
			]
		] 
		button "Load" #"l" [ 
			if not none? f: request-file/keep/filter ["*.sav"][
				error? try [ 
				l: do load first f
				seed: l/s
				clear wl
				append wl l/w
				replace-wlist ]
			]
		]
		button "Start" #"s" [ random/seed seed battle-do ] 
		return
		button "Remove" #"r" [ if wvalue [ remove find wl wvalue show wlist ]]
		button "Save" #"s" [
			if not none? f: request-file/title/filter "Enter a Filename:" "Save" ["*.sav"][
				l: make object! [
					s: seed
					w: wl
				]
				save first f l
			]
		]

		button "Cancel" #"c" [ unview/only bat1 view/new/title main prog-name]
		return
	] "Battle Setup"
]

battle-do: func [][
	core-init
	error: false
	error2: ""
	warriors: copy []
	foreach file wl [
		compile file
		either not error [
			;print loadfile
			o: origin
			c: code
			n: either "" = wname [ form last to-block file ][ trim wname ]
			append warriors make warrior [ origin: o code: c name: n filename: file]
		][
			inform layout [
				label reform [ last to-block file ":" ]
				text error
				text error2
				button "OK"
			]
			exit
		]
	]
	if empty? warriors [ exit ]

	warriors: random warriors
	core-load-warriors warriors
	;probe core
	;foreach w warriors [ print w/name ]

	unview/only bat1
	if Settings/Core-Display [
		display-init
	]	
	if Settings/Core-Disassemble [
		disas-init
	]
	
	labels: copy [ ]
	info: copy []
	foreach w warriors [
		no: index? find warriors w
		c: pick colors ((no - 1) // 8) + 1
		append labels lay: layout/offset [
			styles skins
			origin 50x0
			across
			lab: label 220
			inf: label 50x20 "1"
		] to-pair reduce [0 20 * (no - 1) ]
		lab/text: w/name
		lab/color: c
		lay/color: none
		append info inf
	]
	;view/new first labels
	;probe info
			

	con: layout [
		styles skins
		origin 0x0
		across
		label "Speed" slider 200x16 [ 
			clock/rate: speed-indicator/text: to-integer (value * 99 + 1) 
			show speed-indicator
			show clock
		] 
		speed-indicator: label 50x16"1"
		return
		button "Start" [ mars/started: on if not Settings/Core-Display [while [mars/started][cycle-do]]]
		button "Pause" [ mars/started: off ]
		arrow 24x24 right keycode [ right ] [ cycle-do ]
		button "Cancel" [ 
			mars/started: off unview/only batcon 
			if Settings/Core-Display [
				unview/only grid
			]
			if Settings/Core-Disassemble [
				unview/only disas
			]
			view/new/title main prog-name
		]
	]
	con/color: none

	clock: make face [
		text: "0000"
		offset: 159x1
		feel: make feel [
			engage: func [face action event /local i][
				either mars/started [
					cycle-do
				][ tick: 0
				]
			]
		]
		after: none
		rate: 1
	]

	siz: to-pair reduce [ 370 20 * length? info ]
	batcon: layout [
		styles skins backdrop-norm
		panel1: box :siz
		panel2: box 390x100
		panel3: box 1x1
	]
	panel1/pane: reduce labels
	panel2/pane: reduce [ con ]
	panel3/pane: reduce [ clock ]
	view/new/title batcon "Battle Control"
			

]

tournament: func [][
	inform layout [
		styles skins backdrop-norm
		banner "Tournament"
		label "#1 on ToDo list"
		button "OK"
	]
]

settin: func [][
	sett: layout [
		styles skins backdrop-norm
		style lab label 200 right
		vh2 "Settings"
		across
		label "Core" return
across
lab "Core-Size" lCore-Size: field form Settings/Core-Size return
lab "Cycles-Before-Tie" lCycles-Before-Tie: field form Settings/Cycles-Before-Tie return
lab "Initial-Instruction" lInitial-Instruction: choice data ["DAT.F $0, $0" "NONE" "RANDOM"] return
lab "Instruction-Limit" lInstruction-Limit: field form Settings/Instruction-Limit return
lab "Maximum-Number-of-Tasks" lMaximum-Number-of-Tasks: field form Settings/Maximum-Number-of-Tasks return
lab "Minimum-Separation" lMinimum-Separation: field form Settings/Minimum-Separation return
lab "Read-Distance" lRead-Distance: field form Settings/Read-Distance return
lab "Separation" lSeparation: field form Settings/Separation return
lab "Write-Distance" lWrite-Distance: field form Settings/Write-Distance return	
		
		label "Display" return
across
lab "Core-Display" lCore-Display: check return
lab "Core-Disassemble" lCore-Disassemble: check return
		
		label "Tournament" return
across
lab "Warriors" lWarriors: field form Settings/Warriors return
		
		
across
lab
		button 96 "Save" #"s" [ 
               		Settings/Core-Size: load lCore-Size/text
               		Settings/Cycles-Before-Tie: load lCycles-Before-Tie/text
               		Settings/Initial-Instruction: lInitial-Instruction/text
               		Settings/Instruction-Limit: to-integer (load lInstruction-Limit/text) // ((load lCore-Size/text) / (load lWarriors/text) + 1 )
               		Settings/Maximum-Number-of-Tasks: load lMaximum-Number-of-Tasks/text
               		if error? try [ Settings/Minimum-Separation: (to-integer lMinimum-Separation/text) // (((load lCore-Size/text) / load lWarriors/text) - Settings/Instruction-Limit) 
			][ Settings/Minimum-Separation: 0 ]
               		Settings/Read-Distance: load lRead-Distance/text
			if error? try [
				Settings/Separation: to-integer lSeparation/text
			][
				Settings/Separation: "RANDOM"
			]
               		Settings/Warriors: load lWarriors/text
               		Settings/Write-Distance: load lWrite-Distance/text
			Settings/Core-Display: lCore-Display/data
			Settings/Core-Disassemble: lCore-Disassemble/data
			reload-script: off
			save %settings.txt settings
			unview/only sett
			if reload-script [ ]
			view/new/title main prog-name
		]
		button 96 "Cancel" #"c" [ unview/only sett view/new/title main prog-name]
		return

		]
	lInitial-Instruction/text: Settings/Initial-Instruction
	lCore-Display/data: Settings/Core-Display
	lCore-Disassemble/data: Settings/Core-Disassemble
	unview/only main
	view/new/title sett "Settings"

]

credits: func [][
	view/new/title cred: layout [
		styles skins backdrop-norm
		style lab label 200 right
		style lab2 label 200 left
		banner "Credits"
		across
		lab "Author:" lab2 "Pawe³ GAWROÑSKI" return
		lab "Home screen gfx:" lab2 "Robert £OTOCKI" return
		return
		lab "License" lab2 "MIT" return
		lab "Contact: hegemon@sgh.waw.pl" 
		button "Close" #"c" [ unview/only cred ]
	] "Credits"
]

help-me: does [
	browse http://www.ecst.csuchico.edu/~pizza/koth/icws94.html
]

main: layout [
	styles skins backdrop-norm
	banner "Hegemon's Arbiter"
	image 315x315 %back.jpg ibevel
	across
	button "Battle" #"b" [ battle ]
	button "Tournament" #"t" [ tournament ] 
	button "Help" #"h" [ help-me ] return
	button "Settings" #"s" [ settin ] 
	button "Credits" #"c" [ credits ] 
	button "Quit" #"q" [ quit ]
]

view/title main prog-name
