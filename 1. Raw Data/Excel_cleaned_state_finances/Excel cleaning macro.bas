Attribute VB_Name = "Module4"
Sub finalmacro()
Attribute finalmacro.VB_ProcData.VB_Invoke_Func = " \n14"
'
' finalmacro Macro
'

'
    Rows("1:10").Select
    Selection.Delete Shift:=xlUp
    Cells.Select
    Cells.EntireColumn.AutoFit
    Columns("A:A").Select
    Selection.Delete Shift:=xlToLeft
    Columns("B:F").Select
    Selection.Delete Shift:=xlToLeft
    Range("B1:IV1").Select
    ActiveWindow.SmallScroll ToRight:=2
    Selection.Cut Destination:=Range("D1:IX1")
    Range("D1:IX1").Select
    ActiveWindow.ScrollColumn = 238
    ActiveWindow.ScrollColumn = 237
    ActiveWindow.ScrollColumn = 236
    ActiveWindow.ScrollColumn = 235
    ActiveWindow.ScrollColumn = 234
    ActiveWindow.ScrollColumn = 233
    ActiveWindow.ScrollColumn = 232
    ActiveWindow.ScrollColumn = 229
    ActiveWindow.ScrollColumn = 226
    ActiveWindow.ScrollColumn = 221
    ActiveWindow.ScrollColumn = 216
    ActiveWindow.ScrollColumn = 210
    ActiveWindow.ScrollColumn = 206
    ActiveWindow.ScrollColumn = 203
    ActiveWindow.ScrollColumn = 199
    ActiveWindow.ScrollColumn = 197
    ActiveWindow.ScrollColumn = 196
    ActiveWindow.ScrollColumn = 195
    ActiveWindow.ScrollColumn = 194
    ActiveWindow.ScrollColumn = 193
    ActiveWindow.ScrollColumn = 192
    ActiveWindow.ScrollColumn = 191
    ActiveWindow.ScrollColumn = 190
    ActiveWindow.ScrollColumn = 188
    ActiveWindow.ScrollColumn = 187
    ActiveWindow.ScrollColumn = 186
    ActiveWindow.ScrollColumn = 185
    ActiveWindow.ScrollColumn = 184
    ActiveWindow.ScrollColumn = 183
    ActiveWindow.ScrollColumn = 182
    ActiveWindow.ScrollColumn = 181
    ActiveWindow.ScrollColumn = 180
    ActiveWindow.ScrollColumn = 179
    ActiveWindow.ScrollColumn = 178
    ActiveWindow.ScrollColumn = 177
    ActiveWindow.ScrollColumn = 176
    ActiveWindow.ScrollColumn = 175
    ActiveWindow.ScrollColumn = 174
    ActiveWindow.ScrollColumn = 172
    ActiveWindow.ScrollColumn = 171
    ActiveWindow.ScrollColumn = 170
    ActiveWindow.ScrollColumn = 169
    ActiveWindow.ScrollColumn = 168
    ActiveWindow.ScrollColumn = 167
    ActiveWindow.ScrollColumn = 166
    ActiveWindow.ScrollColumn = 165
    ActiveWindow.ScrollColumn = 164
    ActiveWindow.ScrollColumn = 163
    ActiveWindow.ScrollColumn = 162
    ActiveWindow.ScrollColumn = 161
    ActiveWindow.ScrollColumn = 160
    ActiveWindow.ScrollColumn = 159
    ActiveWindow.ScrollColumn = 158
    ActiveWindow.ScrollColumn = 157
    ActiveWindow.ScrollColumn = 156
    ActiveWindow.ScrollColumn = 155
    ActiveWindow.ScrollColumn = 154
    ActiveWindow.ScrollColumn = 153
    ActiveWindow.ScrollColumn = 152
    ActiveWindow.ScrollColumn = 151
    ActiveWindow.ScrollColumn = 150
    ActiveWindow.ScrollColumn = 149
    ActiveWindow.ScrollColumn = 148
    ActiveWindow.ScrollColumn = 147
    ActiveWindow.ScrollColumn = 146
    ActiveWindow.ScrollColumn = 145
    ActiveWindow.ScrollColumn = 144
    ActiveWindow.ScrollColumn = 143
    ActiveWindow.ScrollColumn = 142
    ActiveWindow.ScrollColumn = 141
    ActiveWindow.ScrollColumn = 140
    ActiveWindow.ScrollColumn = 139
    ActiveWindow.ScrollColumn = 138
    ActiveWindow.ScrollColumn = 137
    ActiveWindow.ScrollColumn = 136
    ActiveWindow.ScrollColumn = 135
    ActiveWindow.ScrollColumn = 134
    ActiveWindow.ScrollColumn = 133
    ActiveWindow.ScrollColumn = 132
    ActiveWindow.ScrollColumn = 131
    ActiveWindow.ScrollColumn = 130
    ActiveWindow.ScrollColumn = 129
    ActiveWindow.ScrollColumn = 128
    ActiveWindow.ScrollColumn = 127
    ActiveWindow.ScrollColumn = 126
    ActiveWindow.ScrollColumn = 125
    ActiveWindow.ScrollColumn = 124
    ActiveWindow.ScrollColumn = 123
    ActiveWindow.ScrollColumn = 122
    ActiveWindow.ScrollColumn = 121
    ActiveWindow.ScrollColumn = 120
    ActiveWindow.ScrollColumn = 119
    ActiveWindow.ScrollColumn = 118
    ActiveWindow.ScrollColumn = 117
    ActiveWindow.ScrollColumn = 116
    ActiveWindow.ScrollColumn = 115
    ActiveWindow.ScrollColumn = 114
    ActiveWindow.ScrollColumn = 113
    ActiveWindow.ScrollColumn = 112
    ActiveWindow.ScrollColumn = 111
    ActiveWindow.ScrollColumn = 109
    ActiveWindow.ScrollColumn = 106
    ActiveWindow.ScrollColumn = 104
    ActiveWindow.ScrollColumn = 101
    ActiveWindow.ScrollColumn = 99
    ActiveWindow.ScrollColumn = 98
    ActiveWindow.ScrollColumn = 96
    ActiveWindow.ScrollColumn = 95
    ActiveWindow.ScrollColumn = 94
    ActiveWindow.ScrollColumn = 93
    ActiveWindow.ScrollColumn = 92
    ActiveWindow.ScrollColumn = 90
    ActiveWindow.ScrollColumn = 89
    ActiveWindow.ScrollColumn = 87
    ActiveWindow.ScrollColumn = 86
    ActiveWindow.ScrollColumn = 85
    ActiveWindow.ScrollColumn = 84
    ActiveWindow.ScrollColumn = 82
    ActiveWindow.ScrollColumn = 78
    ActiveWindow.ScrollColumn = 73
    ActiveWindow.ScrollColumn = 65
    ActiveWindow.ScrollColumn = 56
    ActiveWindow.ScrollColumn = 50
    ActiveWindow.ScrollColumn = 42
    ActiveWindow.ScrollColumn = 35
    ActiveWindow.ScrollColumn = 28
    ActiveWindow.ScrollColumn = 23
    ActiveWindow.ScrollColumn = 17
    ActiveWindow.ScrollColumn = 13
    ActiveWindow.ScrollColumn = 10
    ActiveWindow.ScrollColumn = 7
    ActiveWindow.ScrollColumn = 5
    ActiveWindow.ScrollColumn = 4
    ActiveWindow.ScrollColumn = 3
    ActiveWindow.ScrollColumn = 2
    ActiveWindow.ScrollColumn = 1
    Range("H1").Select
    Cells.Find(What:="dis", After:=ActiveCell, LookIn:=xlFormulas, LookAt:= _
        xlPart, SearchOrder:=xlByRows, SearchDirection:=xlNext, MatchCase:=False _
        , SearchFormat:=False).Activate
    Range("AR1").Select
    Selection.Cut Destination:=Range("AS1")
    Range("AS1").Select
    Columns("AS:AS").ColumnWidth = 11.29
    Columns("AS:AS").EntireColumn.AutoFit
    ActiveWindow.ScrollColumn = 34
    ActiveWindow.ScrollColumn = 33
    ActiveWindow.ScrollColumn = 32
    ActiveWindow.ScrollColumn = 30
    ActiveWindow.ScrollColumn = 29
    ActiveWindow.ScrollColumn = 28
    ActiveWindow.ScrollColumn = 27
    ActiveWindow.ScrollColumn = 25
    ActiveWindow.ScrollColumn = 23
    ActiveWindow.ScrollColumn = 22
    ActiveWindow.ScrollColumn = 20
    ActiveWindow.ScrollColumn = 17
    ActiveWindow.ScrollColumn = 14
    ActiveWindow.ScrollColumn = 11
    ActiveWindow.ScrollColumn = 8
    ActiveWindow.ScrollColumn = 4
    ActiveWindow.ScrollColumn = 2
    ActiveWindow.ScrollColumn = 1
    Range("B:C,E:H,J:M,O:R,T:W").Select
    Range("T1").Activate
    Selection.Delete Shift:=xlToLeft
    Range("G:J,L:O,Q:T,V:Z,AB:AD,AF:AI,AK:AN").Select
    Range("AK1").Activate
    Selection.Delete Shift:=xlToLeft
    ActiveWindow.SmallScroll ToRight:=-13
    Range("N:Q,S:V,X:AA,AC:AF,AH:AK,AM:AP").Select
    Range("AM1").Activate
    Selection.Delete Shift:=xlToLeft
    Range("Y:AB,AD:AG,AI:AL,AN:AQ").Select
    Range("AN1").Activate
    Selection.Delete Shift:=xlToLeft
    ActiveWindow.SmallScroll ToRight:=-6
    Columns("T:W").Select
    Selection.Delete Shift:=xlToLeft
    Range("Y:AB,AD:AG").Select
    Range("AD1").Activate
    Selection.Delete Shift:=xlToLeft
    Range("AA:AD,AF:AI").Select
    Range("AF1").Activate
    Selection.Delete Shift:=xlToLeft
    Range("AC:AF,AH:AK").Select
    Range("AH1").Activate
    Selection.Delete Shift:=xlToLeft
    ActiveWindow.SmallScroll ToRight:=11
    Range("AE:AH,AJ:AM,AO:AR").Select
    Range("AO1").Activate
    Selection.Delete Shift:=xlToLeft
    Range("AH:AK,AM:AP,AR:AU").Select
    Range("AR1").Activate
    Selection.Delete Shift:=xlToLeft
    Range("AK:AN,AP:AS").Select
    Range("AP1").Activate
    Selection.Delete Shift:=xlToLeft
    ActiveWindow.SmallScroll ToRight:=9
    Range("AM:AP,AR:AU,AW:AZ,BB:BE").Select
    Range("BB1").Activate
    Selection.Delete Shift:=xlToLeft
    Range("AQ:AT,AV:AY,BA:BD").Select
    Range("BA1").Activate
    Selection.Delete Shift:=xlToLeft
    Range("AT:AW,AY:BB").Select
    Range("AY1").Activate
    Selection.Delete Shift:=xlToLeft
    Range("AV:AY,BA:BD").Select
    Range("BA1").Activate
    Selection.Delete Shift:=xlToLeft
    Range("AX:BA,BC:BF").Select
    Range("BC1").Activate
    ActiveWindow.SmallScroll ToRight:=10
    Range("AX:BA,BC:BF,BH:BK,BM:BN").Select
    Range("BM1").Activate
    Selection.Delete Shift:=xlToLeft
    ActiveWindow.ScrollColumn = 49
    ActiveWindow.ScrollColumn = 48
    ActiveWindow.ScrollColumn = 46
    ActiveWindow.ScrollColumn = 45
    ActiveWindow.ScrollColumn = 43
    ActiveWindow.ScrollColumn = 40
    ActiveWindow.ScrollColumn = 36
    ActiveWindow.ScrollColumn = 32
    ActiveWindow.ScrollColumn = 27
    ActiveWindow.ScrollColumn = 20
    ActiveWindow.ScrollColumn = 13
    ActiveWindow.ScrollColumn = 6
    ActiveWindow.ScrollColumn = 1
    Rows("2:5").Select
    Selection.Delete Shift:=xlUp
    Range("A1").Select
    ActiveCell.FormulaR1C1 = "State"
    Range("A2").Select
    ActiveCell.FormulaR1C1 = "Year"
    Range("B2").Select
    ActiveCell.FormulaR1C1 = "2019"
    Selection.AutoFill Destination:=Range("B2:AZ2"), Type:=xlFillDefault
    Range("B2:AZ2").Select
    ActiveWindow.ScrollColumn = 34
    ActiveWindow.ScrollColumn = 33
    ActiveWindow.ScrollColumn = 29
    ActiveWindow.ScrollColumn = 21
    ActiveWindow.ScrollColumn = 13
    ActiveWindow.ScrollColumn = 1
    Range("4:4,6:6,11:11,27:27").Select
    Range("A27").Activate
    ActiveWindow.SmallScroll Down:=27
    Range("4:4,6:6,11:11,27:27,44:44").Select
    Range("A44").Activate
    ActiveWindow.SmallScroll Down:=9
    Range("4:4,6:6,11:11,27:27,44:44,50:50,56:56,58:58,64:64").Select
    Range("A64").Activate
    Selection.Delete Shift:=xlUp
    ActiveWindow.SmallScroll Down:=-9
    Range("D58").Select
    ActiveWindow.SmallScroll Down:=-54
End Sub
