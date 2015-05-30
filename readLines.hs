main = interact linesCount
		where linesCount input = show (length (lines input) ) ++ "\n"