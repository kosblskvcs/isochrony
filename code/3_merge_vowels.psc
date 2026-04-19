select all
Remove

directory$ = "C:\Users\pplssupport\OneDrive - University of Edinburgh\dissertation\isochrony_data\export_isochrony_"

for p from 11 to 11
    if p < 10
        participant$ = "0'p'" 
    else 
        participant$ = "'p'" 
    endif
    printline 'participant$'
    
    folder$ = "'directory$''participant$'"
    # concatenating wave files
    Create Strings as file list: "textgridList",  "'folder$'" + "\File*.TextGrid"
    n_tg = Get number of strings
    printline 'n_tg'

    for i from 1 to n_tg
        select Strings textgridList
        stringx$ = Get string: i
        Read from file: "'folder$'" + "\'stringx$'" - ".TextGrid" + "_Track1.wav"
    endfor

    select all
        minusObject: "Strings textgridList"
    Concatenate recoverably

    selectObject: "Sound chain"
    Save as WAV file: "'folder$'" + "\sound_chain_'participant$'_vowels" + ".wav"

    select all
        minusObject: "Strings textgridList"
	minusObject: "TextGrid chain"
    Remove
    selectObject: "TextGrid chain"
	Rename: "files"

    # concatenating textgrids

    for i from 1 to n_tg
        select Strings textgridList
        stringx$ = Get string: i
	fileNum$ = mid$ (stringx$, 5, 3)
	Read from file: "'folder$'" + "\'stringx$'"
    endfor
    
    # in some cases, there are rounding errors at the microsecond level, but only in the case of the last tg to be concatenated, possibly resulting from splitting
    # it is always the second (ultrasound) tier which is (only) _somewhat_ shorter; this is why we have to chunk the whole tg to the duration of the first tier
    dur1 = Get total duration of intervals where: 1, "matches (regex)", ".*"
    Extract part: 0, dur1, "no"
    selectObject: "TextGrid File'fileNum$'"
    Remove
    selectObject: "TextGrid File'fileNum$'_part"
    Rename: "File'fileNum$'"

    # now, we can eventually do the concatenation
    select all
        minusObject: "Strings textgridList"
	minusObject: "TextGrid files"
    Concatenate

    selectObject: "TextGrid chain"
	plusObject: "TextGrid files"
    Merge
    Save as text file: "'folder$'" + "\merged_'participant$'_vowels" + ".TextGrid"

    select all
    Remove

endfor