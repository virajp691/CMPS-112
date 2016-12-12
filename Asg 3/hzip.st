#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"hzip.st -dtcu inputfile [outputfile]"

"Parse command line arguments"

nl := Character nl.

"--- FileStream Extend ---"
FileStream extend [
    |bitIndex currentChar|
    nextBit [
        |bit|
            bitIndex isNil ifTrue: [bitIndex := 0].
            bitIndex = 0 ifTrue: [
                bitIndex := 8.
                currentChar := self next.
            ].
            bit := currentChar value bitAt: bitIndex.
            bitIndex := bitIndex -1.
            ^ bit.
    ]
    atBitEnd [
        ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
    ]
    backBit [
        bitIndex := bitIndex + 1.
    ]
]

"--- BitStream Object ---"
Object subclass: BitStream [
    |bitIndex currentByte myStream|
    BitStream class >> new [
        self shouldNotImplement.
    ]
    BitStream class >> new: fileStream [
        |result|
        result := super new.
        result init: fileStream.
        ^result
    ]
    init: fileStream [
        myStream := fileStream.
        self clearByte.
    ]
    writeBit: bit [
        currentByte := currentByte bitAt: bitIndex put: bit.
        bitIndex := bitIndex - 1.
        bitIndex = 0 ifTrue: [self flushByte].
    ]
    writeBitString: bitString [
        bitString do: [:index |
            index asString = '0'
                ifTrue: [self writeBit: 0]
                ifFalse: [self writeBit: 1].
        ].
    ]
    clearByte [
        bitIndex := 8.
        currentByte := 0.
    ]
    flushByte [
        bitIndex = 8 ifFalse: [
            myStream nextPutByte: currentByte.
            self clearByte.
        ]
    ]
]

"--- Leaf Object ---"
Object subclass: Leaf [
    |char count|
    char [^ char ]
    count [^ count ]
    isLeaf [^ true ]
    Leaf class >> new [
        self shouldNotImplement.
    ]
    Leaf class >> new: aChar count: aCount [
        |result|
        result := super new.
        result setChar: aChar andCount: aCount.
        ^result
    ]
    setChar: aChar andCount: aCount [
        char := aChar.
        count := aCount.
    ]
    getChar [
        ^ char.
    ]
    getCount [
        ^ count.
    ]
    <= other [
        ^ (count < other count)
        | ((count = other count) & (char <= other char))
    ]
    printBase: aStream [
        ^ aStream << self class << '(' << char << ',' << count
    ]
    printOn: aStream [
        (self printBase: aStream) << ')'.
    ]
    inorder: visitor prefix: string [
        visitor value: char value: string.
    ]
    binTreeDump: aBitstream [
        |asciicode|
        aBitstream writeBit: 0.
        asciicode := char asciiValue.
        8 to: 1 by: -1 do: [:index |
            |currentBit|
            currentBit := asciicode bitAt: index.
            aBitstream writeBit: currentBit.
        ].
        asciicode = 256 ifTrue: [
            aBitstream writeBit: 1.
        ].
    ]
]

"--- Tree Object ---"
Leaf subclass: Tree [
    |left right|
    Tree class >> new: aChar count: aCount [
        self shouldNotImplement
    ]
    Tree class >> new: aChar count: aCount left: aLeft right: aRight [
        |result|
        result := super new: aChar count: aCount.
        result setLeft: aLeft andRight: aRight.
        ^ result
    ]
    isLeaf [ ^ false ]
    setLeft: aLeft andRight: aRight [
        left := aLeft.
        right := aRight.
    ]
    left [ ^ left ]
    right [^ right ]
    printOn: aStream [
        (self printBase: aStream) << ',' << left << ',' << right << ')'.
    ]
    inorder: visitor prefix: string [
        left inorder: visitor prefix: string, '0'.
        right inorder: visitor prefix: string, '1'.
    ]
    binTreeDump: aBitstream [
        left binTreeDump: aBitstream.
        right binTreeDump: aBitstream.
        aBitstream writeBit: 1.
    ]
]
        
"--- Frequency Table Object ---"
Object subclass: ZeroArray [
    |theArray|
    ZeroArray class >> new [
        self shouldNotImplement.
    ]
    ZeroArray class >> new: size [
        |result|
        result := super new.
        result init: size.
        ^ result
        ]
    init: size [
        ^ theArray := Array new: size.
    ]
    size [
        ^ theArray size.
    ]
    at: index [
        ^ theArray at: index + 1.
    ]
    at: index put: value [
        ^ theArray at: index + 1 put: value.
    ]
    incr: index [
        (self at: index) isNil
            ifTrue: [ self at: index put: 0].
        self at: index put: (self at: index) + 1.
    ]
    keysAndValuesDo: aBlock [
        (0 to: (self size) - 1) do: [:index |
            aBlock value: index value: (self at: index).
        ]
    ]
]

"--- Test if proper number of arguments ---"
(((Smalltalk arguments size) < 2) | ((Smalltalk arguments size) > 3))
    ifTrue: [
        'Invalid Arguments' displayNl.
        ObjectMemory quit
    ]. 

"--- Evaluate argument ---"
Smalltalk arguments: '-d -c -t -u'
do: [:opt :arg |
    "--- Option -c ---"
    (opt = $c)
        ifTrue: [
        	'-c command' displayNl.
        	"--- Open input file ---"
            filename:= Smalltalk getArgv: 2.
            outname:= Smalltalk getArgv: 3.
            (outname isNil) ifTrue:
                [outname := stdout].
            infile := FileStream open: filename
                                 mode: FileStream read.

        	"--- Create frequency table ---"
            freqtable := ZeroArray new: 257.

      		"--- Interate through file and push character into table ---"
            [infile atEnd not] whileTrue: [
                |ordChar|
                ordChar := infile next asInteger.
                freqtable incr: ordChar.
            ].
            freqtable at: 256 put: 1.
        	"--- Generate and store leaf nodes from freqtable ---"
            sortcol := SortedCollection new.
            freqtable keysAndValuesDo: [:key :value |
                |node|
                (value notNil and: [value > 0]) ifTrue: [
                    node := Leaf new: key asCharacter count: value.
                    sortcol add: node.
                ]
            ].
		"--- Loop through sortcol removing 2 elements until only one element left ---"
            [sortcol size > 1] whileTrue: [
                |node1 node2 t treecount tc|
                node1 := sortcol removeFirst.
                node2 := sortcol removeFirst.
                tc := node1 char.
                treecount := node1 count + node2 count + 1.
                t := Tree new: tc count: treecount left: node1 right: node2.
                sortcol add: t.
            ].
            "--- Open Outfile ---"
           outfile := FileStream open: outname.
            "--- Build encodetable ---"
            tree := sortcol first.
            encodetable := ZeroArray new: 257.
            tree inorder: [:char :string |
                index := char asInteger.
                encodetable at: index put: string.
            ] prefix: ''.
                
            "--- Compress ---"
            outBytes := BitStream new: outfile.
            tree binTreeDump: outBytes.
            outBytes writeBit: 1.
            
            "--- Insert Paths of characters ---"
            infile reset.
            [infile atEnd not] whileTrue: [
                |ordChar|
                ordChar := infile next asInteger.
                encString := encodetable at: ordChar.
                outBytes writeBitString: encString.
            ].
            eofString := encodetable at: 256.
            outBytes writeBitString: eofString.
            outBytes flushByte.
            infile close.
            outfile close.
        ].

        "--- Option -t ---"
	(opt = $t)
		ifTrue: [
			'-t command' displayNl.
        	"--- Open input file ---"
            filename:= Smalltalk getArgv: 2.
            infile := FileStream open: filename
                                 mode: FileStream read.

        	"--- Create frequency table ---"
            freqtable := ZeroArray new: 257.

      		"--- Interate through file and push character into table ---"
            [infile atEnd not] whileTrue: [
                |ordChar|
                ordChar := infile next asInteger.
                freqtable incr: ordChar.
            ].
            freqtable at: 256 put: 1.
        	"--- Generate and store leaf nodes from freqtable ---"
            sortcol := SortedCollection new.
            freqtable keysAndValuesDo: [:key :value |
                |node|
                (value notNil and: [value > 0]) ifTrue: [
                    node := Leaf new: key asCharacter count: value.
                    sortcol add: node.
                ]
            ].
	    "--- Loop through sortcol removing 2 elements until only one element left ---"
            [(sortcol size) > 1] whileTrue: [
                |node1 node2 treecount tc|
                node1 := sortcol removeFirst.
                node2 := sortcol removeFirst.
                tc := node1 char.
                treecount := node1 count + node2 count + 1.
                t := Tree new: tc count: treecount left: node1 right: node2.
                sortcol add: t.
            ].
            tree := sortcol first.
            encodetable := ZeroArray new: 257.
            tree inorder: [:char :string |
                index := char asInteger.
                encodetable at: index put: string.
            ] prefix: ''.

            encodetable keysAndValuesDo: [:key :value |
                (value notNil and: [value > 0]) ifTrue: [
                    |index char count string pad|
                    key = 10 ifTrue: [
                        char := '\n '.
                    ] ifFalse: [
                        key = 256 ifTrue: [
                            char := 'EOF'.
                        ] ifFalse: [
                            char := key asCharacter asString, '  '.
                        ].
                    ].
                    stdout << char << ' '.
                    count := (freqtable at: key) printString.
                    pad := 5 - count size.
                    [pad > 0] whileTrue: [
                        count := count,' '.
                        pad := pad - 1.
                    ].
                    stdout << count << ' '.
                    string := value.
                    stdout << string << nl.
                ].
            ].
	].
    "--- Uncompressing ---"
        (opt = $u) ifTrue: [
            stdout << '-u command' << nl.
            infilename := Smalltalk getArgv: 2.
            infile := FileStream open: infilename mode: FileStream read.
            outfilename := Smalltalk getArgv: 3.
            (outfilename isNil not) ifTrue: [
                outfile := FileStream open: outfilename.
            ].
            currByte := 0.
            rehydrate := false.
            stack := OrderedCollection new.

            "--- Iterate through file bit-by-bit ---"
            [(infile atBitEnd not) & (rehydrate = false)] whileTrue: [
                |currBit|
                currBit := infile nextBit.
                currBit = 0 ifTrue: [
                    currByte := 0.
                    "--- Rebuild Byte ---"
                    8 to: 1 by: -1 do: [:index |
                        currBit := infile nextBit.
                        currByte := currByte bitAt: index put: currBit.
                    ].
                    currByte = 0 ifTrue: [
                        currBit := infile nextBit.
                        currBit = 1 ifTrue: [
                            currByte := currByte + 256.
                        ].
                    ].
                    node := Leaf new: currByte asCharacter count: 1.
                    stack addFirst: node.
                ] ifFalse: [
                    stack size = 1 ifTrue: [
                        rehydrate := true.
                    ] ifFalse: [
                        node1 := stack removeFirst.
                        node0 := stack removeFirst.
                        tree := Tree new: (node1 char) count: 1 left: node0 right: node1.
                        stack addFirst: tree.
                    ].
                ].
            ].
            root := stack removeFirst.
            node := root.
            eof := false.
            [(infile atBitEnd not) & (eof = false)] whileTrue: [
                |ebit|
                ebit := infile nextBit.
                ebit = 0 ifTrue: [
                    node isLeaf ifTrue: [
                        node char asInteger = 256 ifTrue: [eof := true.].
                    ] ifFalse: [
                        node := node left.
                        node isLeaf ifTrue: [
                            node char asInteger = 256
                                ifTrue: [eof := true.]
                                ifFalse: [
                                    stdout << node char.
                                    (outfilename isNil not) ifTrue: [
                                        outfile << node char.
                                    ].
                                ].
                            node := root.
                        ].

                    ].
                ] ifFalse: [
                    node isLeaf ifTrue: [
                        node char asInteger = 256 ifTrue: [eof := true.].
                    ] ifFalse: [
                        node := node right.
                        node isLeaf ifTrue: [
                            node char asInteger = 256
                                ifTrue: [eof := true.]
                                ifFalse: [
                                    stdout << node char.
                                    (outfilename isNil not) ifTrue: [
                                        outfile << node char.
                                    ].
                            ].
                            node := root.
                        ].
                    ].
                ].
            ].
        ].
]
ifError: [
'Error: invalid option' displayNl.
'Usage hzip -[cd] inputfile [outputfile]' displayNl.
].
