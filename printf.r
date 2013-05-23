REBOL [
    Title:      "printf and sprintf functions"
    Date:       2/8/1999
    File:       %printf.r
    Author:     "Thomas Jensen"
    Email:      dm98411@edb.tietgen.dk

    Purpose:    {
        Supplies c-like printf and sprintf functions
    }

    Category:   'general

    Example: {
        printf "Hello %s\n" username
        printf "%-10d%-10f\n" [10 20.5]
        my-string: sprintf "The value of pi is %.7f" pi
    }

    Comment: {
        the format is %[-][width][.decimals]type
        -        forces left justify instead of right
        width    is an optional total width of field
        decimals specify the number of decimals in floating point numbers
        type is one of:
            s    string (argument can be any REBOL type)
            d    integer number
            f    floating point number (ie. decimal)
            %    prints a %
    }
]


; when test-mode is on, a small example is given
test-mode: off



format-type: function [value type len sublen justify] [
    integer-part decimal-part
    spaces-where
] [

    value: form value

    if :type = 'decimal [
        decimal-part: none
        parse value [copy integer-part [to #"." | to end] thru #"." copy decimal-part to end]
        if not decimal-part [decimal-part: "0"]
        if sublen [
            sublen: load sublen
            decimal-part: copy/part decimal-part sublen
            insert/dup tail decimal-part #"0" (sublen - (length? decimal-part))
        ]
        value: rejoin [integer-part #"." decimal-part]
    ]

    if len [
        len: load len
        spaces-where: select [
            right   head
            left    tail
        ] justify

        insert/dup (do spaces-where value) #" " (len - (length? value))
    ]

    value
]


sprintf: function [
    "Format a string given an argument block and return the result as a string"
    fmtstr [string!]    "Format string"
    args                "Arguments"
][
    numbers types chars all-chars
    out
    type-lookup
    len sublen
][
    numbers: charset [#"0" - #"9"]
    types: charset "sfd"
    chars: complement charset "%\"
    all-chars: complement charset ""
    out: make string! ""

    if not block? args [
        args: reduce [args]
    ]

    args: reduce args

    type-lookup: [
        "f" 'decimal
        "d" 'integer
        "s" 'string
    ]

    parse/all fmtstr [
        some [
            [
                copy str some chars (append out str)
            ] |
            [
                "%%"
                (append out #"%")
            ] |
            [
                #"%"
                (len: none)
                (sublen: none)
                (justify: 'right)
                [#"-" (justify: 'left) | none]
                [copy len some numbers | none]
                [#"." copy sublen some numbers | none]
                [
                    copy type 1 types
                    (
                        append out format-type first args (select type-lookup type) len sublen justify
                        args: next args
                    )
                    | skip 1 all-chars
                ]
            ] |
            [
                #"\"
                [copy modifier 1 all-chars]
                (
                    append out select [
                        "n" "^/"
                        "t" "^-"
                        "\" "\"
                    ] modifier
                )
            ]
        ]
    ]
    out
]

printf: func [
    "Format and print a string given an argument block"
    fmtstr [string!]    "Format string"
    args                "Argument(s)"
] [
    prin sprintf fmtstr args
]


if test-mode [
    print "Test mode is on"
    printf {Edit the line: "test-mode: on" in %s to switch it off\n} system/script/header/file

    print "Listing current directory"

    current-dir: load %./  

    printf "%-20s%15s%5s\n" ["Filename" "Modified" "Age"]
    foreach file current-dir [
        modified: modified? to-file file
        age: now - modified
        printf "%-20s%15s%5s\n" [file modified/date age]
    ]
]