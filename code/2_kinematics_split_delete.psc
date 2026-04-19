form Script to read files into the Objects window
	sentence directory C:\Users\pplssupport\OneDrive - University of Edinburgh\dissertation\isochrony_data\export_isochrony_
	#sentence participant 04
	sentence inputFolder \tg_from_BAS_
endform

for p from 2 to 2
if p < 10
    participant$ = "0'p'"
else
    participant$ = "'p'"
endif
Read from file: "'directory$'" + "'participant$'" + "'inputFolder$'" + "'participant$'" + "\merged_" + "'participant$'" + ".TextGrid"

# new tier for annotating where kinematics should be extracted from AAA
Insert interval tier: 5, "ultrasound"
n1_intervals = Get number of intervals: 1
n2_intervals = Get number of intervals: 2

# in the orthographic tier, mark the middle of the word "write" which always precedes the target word as well as mark the start of the subsequent word "one" 
for intervalx from 1 to n2_intervals
    labelx$ = Get label of interval: 2, intervalx
    if labelx$ = "write"
        pre_start = Get start point: 2, intervalx
        pre_end = Get end point: 2, intervalx
        pre_mid = pre_start + (pre_end - pre_start)/2
        Insert boundary: 5, pre_mid
    endif
    if labelx$ = "one"
	labelPrevious$ = Get label of interval: 2, intervalx-1
	if labelPrevious$ = ""
	    post_start = Get start point: 2, intervalx-1
	else
            post_start = Get start point: 2, intervalx
	endif
	Insert boundary: 5, post_start
    endif
endfor

# label every second tier in the "ultrasound" tier as the places where the respective kinematics should be extracted from AAA
n5_intervals = Get number of intervals: 5
for i from 1 to (n5_intervals-1)/2
    Set interval text: 5, 2*i, "kinematics"
endfor

# save the merged textgrid which now also has the ultrasound tier
Save as text file: "'directory$'" + "'participant$'" + "'inputFolder$'" + "'participant$'" + "\merged_kinematics_" + "'participant$'" + ".TextGrid"

# split up the merged tg file into as many textgrids as original sound files / tg's there are
# only extract the ultrasound and the segment tier
for i from 1 to n1_intervals
    selectObject: "TextGrid merged_" + "'participant$'"
    startTime = Get start time of interval: 1, i
    endTime = Get end time of interval: 1, i
    label$ = Get label of interval: 1, i
    
    # Create a new TextGrid for this interval
    selectObject: "TextGrid merged_" + "'participant$'"
    Extract part: startTime, endTime, "no"
    
    Extract one tier: 4
    selectObject: "TextGrid merged_" + "'participant$'" + "_part"
    Extract one tier: 5
    
    selectObject: "TextGrid ultrasound"
    startKin = Get start time of interval: 1, 2
    endKin = Get end time of interval: 1, 2
    selectObject: "TextGrid MAU"

    startPreviousPlus1 = 0
    while startPreviousPlus1 < startKin
        startPrevious = Get start time of interval: 1, 2
        Remove boundary at time: 1, startPrevious
        startPreviousPlus1 = Get start time of interval: 1, 2
    endwhile
    
    numIntMAU = Get number of intervals: 1
    startAfterMinus1 = 100
    while startAfterMinus1 > endKin
        startAfter = Get start time of interval: 1, numIntMAU
        Remove boundary at time: 1, startAfter
        numIntMAU = numIntMAU - 1
        startAfterMinus1 = Get start time of interval: 1, numIntMAU
    endwhile

    #Extract part: startKin, endKin, "yes"
    #selectObject: "TextGrid MAU_part"
    selectObject: "TextGrid ultrasound"
    	plusObject: "TextGrid MAU"
    Merge
 
    selectObject: "TextGrid merged"
    Replace interval texts: 2, 1, 0, "(...)", "", "literals"
    Save as text file: "'directory$'" + "'participant$'" + "\" + "'label$'" - "_Track1" + ".TextGrid"
    selectObject: "TextGrid MAU"
        plusObject: "TextGrid merged"
        plusObject: "TextGrid merged_" + "'participant$'" + "_part"
        plusObject: "TextGrid ultrasound"
    Remove
    printline Split textgrid part 'i'
endfor

# Delete textgrids of errorful utterances

Read Table from comma-separated file: "C:\Users\pplssupport\OneDrive - University of Edinburgh\dissertation\isochrony_data\stimuli\corrupted_files.txt"
Extract rows where column (text): "p", "is equal to", "'participant$'"
rowNum = Get number of rows
printline 'rowNum'

for i from 1 to rowNum
    selectObject: "Table corrupted_files_'participant$'"
    fileNum$ = Get value: i, "file"
    deleteFile: "'directory$'" + "'participant$'" + "\File'fileNum$'.TextGrid"
    #numTier = Get number of tiers
    #if numTier == 2
    #    Remove tier: 1
    #endif
    #Save as text file: "'directory$'" + "'participant$'" + "\File'fileNum$'.TextGrid"
endfor

endfor