directory$ = "C:\Users\pplssupport\OneDrive - University of Edinburgh\dissertation\isochrony_data\export_isochrony_"
outputFile$ = "'directory$'dlc\vowel_start_end_all_01_02.csv"

writeFileLine: outputFile$, "participant,stimulus,date_time,file,n_of_vowel,start_of_vowel_within_annot,end_of_vowel_within_annot"
for p from 2 to 2
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
            if labelx$ == "1"
                isT$ = Get label of interval: 2, i-1
                if isT$ == "t"
                    start = Get start time of interval: 2, i-1
		    end = Get end time of interval: 2, i-1
                else
                    start = Get start time of interval: 2, i-2
		    end = Get end time of interval: 2, i-2
                endif
            else
		isR$ = Get label of interval: 2, i-1
                if isR$ == "r"
                    start = Get start time of interval: 2, i-2
		    end = Get end time of interval: 2, i-2
                else
                    start = Get start time of interval: 2, i-1
		    end = Get end time of interval: 2, i-1
                endif
            endif
	    midPrev = start + (end-start)/2
	    start = midPrev
            end = Get end time of interval: 2, i
            duration = end - start

            numKin = Get interval at time: 3, start
            numFile = Get interval at time: 1, start
            startKin = Get start time of interval: 3, numKin
            endKin = Get end time of interval: 3, numKin
            durKin = endKin - startKin
            
            start_within = start - startKin - 0*duration
            if start_within < 0
                start_within = 0
            endif
            end_within = end - startKin + 0.1*duration
            if end_within > durKin
                end_within = durKin  
            endif

            fileNum$ = Get label of interval: 1, numFile
            fileNum_short$ = left$ (fileNum$,7)
            Read Strings from raw text file: "'folder$'\'fileNum_short$'.txt"
            stimulus$ = Get string: 1
	    dateTime$ = Get string: 2
            appendFileLine: outputFile$, "en'participant$','stimulus$','dateTime$','fileNum_short$','labelx$','start_within','end_within'"
        endif
    endfor
    select all
    Remove
endfor