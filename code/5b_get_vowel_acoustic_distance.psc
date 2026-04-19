directory$ = "C:\Users\DELL\OneDrive - University of Edinburgh\dissertation\isochrony_data\export_isochrony_"
outputFile$ = "'directory$'dlc\vowel_acoustic_distance_start.csv"

writeFileLine: outputFile$, "participant,stimulus,date_time,file,n_of_vowel,midpoint,duration"
for p from 1 to 23
    if p < 10 	
        participant$ = "0'p'"
    else
        participant$ = "'p'"
    endif 
    folder$ = "'directory$''participant$'"
    Read from file: "'folder$'\merged_'participant$'_vowels_2.TextGrid"

    numInt = Get number of intervals: 2
    for i from 1 to numInt
        selectObject: "TextGrid merged_'participant$'_vowels_2"
        labelx$ =  Get label of interval: 2, i
        if labelx$ == "1" | labelx$ == "2" | labelx$ == "3"
    	    start = Get start time of interval: 2, i
            end = Get end time of interval: 2, i
            duration = end - start

            numFile = Get interval at time: 1, start
            fileNum$ = Get label of interval: 1, numFile
            fileNum_short$ = left$ (fileNum$,7)
            Read Strings from raw text file: "'folder$'\'fileNum_short$'.txt"
            stimulus$ = Get string: 1
	        dateTime$ = Get string: 2

            appendFileLine: outputFile$, "en'participant$','stimulus$','dateTime$','fileNum_short$','labelx$','start','duration'"
        endif
    endfor
    select all
    Remove
endfor