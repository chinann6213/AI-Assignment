; A function that get city than return the cities they are connected to
(DEFUN GET_ADJACENT_CITIES (CITY) (
	COND
	((STRING-EQUAL CITY "ZERIND")
	 `("ORADEA" "ARAD")
	)
	((STRING-EQUAL CITY "ORADEA")
	 `("ZERIND" "SIBIU")
	)
	((STRING-EQUAL CITY "ARAD")
	 `("ZERIND" "SIBIU" "TIMISOARA")
	)
	((STRING-EQUAL CITY "SIBIU")
	 `("ORADEA" "ARAD" "FAGARAS" "RIMNICU VILCEA")
	)
	((STRING-EQUAL CITY "FAGARAS")
	 `("SIBIU" "BUCHAREST")
	)
	((STRING-EQUAL CITY "RIMNICU VILCEA")
	 `("SIBIU" "CRAIOVA" "PITESTI")
	)
	((STRING-EQUAL CITY "TIMISOARA")
	 `("ARAD" "LUGOJ")
	)
	((STRING-EQUAL CITY "LUGOJ")
	 `("TIMISOARA" "MEHADIA")
	)
	((STRING-EQUAL CITY "MEHADIA")
	 `("DROBETA" "LUGOJ")
	)
	((STRING-EQUAL CITY "DROBETA")
	 `("CRAIOVA" "MEHADIA")
	)
	((STRING-EQUAL CITY "CRAIOVA")
	 `("DROBETA" "PITESTI" "RIMNICU VILCEA")
	)
	((STRING-EQUAL CITY "PITESTI")
	 `("CRAIOVA" "RIMNICU VILCEA" "BUCHAREST")
	)
	((STRING-EQUAL CITY "BUCHAREST")
	 `("FAGARAS" "PITESTI" "URZICENI" "GIURGIU")
	)
	((STRING-EQUAL CITY "GIURGIU")
	 `("BUCHAREST")
	)
	((STRING-EQUAL CITY "URZICENI")
	 `("BUCHAREST" "HIRSOVA" "VASLUI")
	)
	((STRING-EQUAL CITY "HIRSOVA")
	 `("URZICENI" "EFORIE")
	)
	((STRING-EQUAL CITY "EFORIE")
	 `("HIRSOVA")
	)
	((STRING-EQUAL CITY "VASLUI")
	 `("IASI" "URZICENI")
	)
	((STRING-EQUAL CITY "NEAMT")
	 `("IASI")
	)
	((STRING-EQUAL CITY "IASI")
	 `("VASLUI" "NEAMT")
	)
))

; orders the list of cities according to alphabet ascending or descending
(DEFUN ORDER (LIST ORDER) (COND
	((STRING-EQUAL ORDER "ASCENDING")
	 (SORT LIST #'STRING-LESSP))
	((STRING-EQUAL ORDER "DESCENDING")
	 (SORT LIST #'STRING-GREATERP))
))

; a class for BFS states
(DEFCLASS BFS () (
	(CLOSED :ACCESSOR GET-CLOSED :WRITER SET-CLOSED :INITFORM (LIST))
	(OPEN :ACCESSOR GET-OPEN :WRITER SET-OPEN :INITFORM (LIST))
	(MAP :ACCESSOR GET-MAP :WRITER SET-MAP :INITFORM (LIST))
	(CURRENT_CHECK :ACCESSOR GET-CURRENT_CHECK :WRITER SET-CURRENT_CHECK :INITFORM "")
	(START :ACCESSOR GET-START :WRITER SET-START :INITFORM "")
	(GOAL :ACCESSOR GET-GOAL :WRITER SET-GOAL :INITFORM "")
	(TEMP_MAP :ACCESSOR GET-TEMP_MAP :WRITER SET-TEMP_MAP :INITFORM (LIST))
    (NAME :ACCESSOR GET-NAME :WRITER SET-NAME :INITFORM "" :INITARG :INIT-NAME)
    (ORDER :ACCESSOR GET-ORDER :WRITER SET-ORDER :INITFORM "")
))

; resets a BFS object
(DEFMETHOD RESET ((OBJECT BFS)) (
	PROGN (SET-CLOSED (LIST) OBJECT)
	(SET-OPEN (LIST) OBJECT)
	(SET-MAP (LIST) OBJECT)
	(SET-CURRENT_CHECK "" OBJECT)
	(SET-START  "" OBJECT)
	(SET-GOAL "" OBJECT)
	(SET-TEMP_MAP (LIST) OBJECT)
))

; combines open and closed list of a BFS object then return it
(DEFMETHOD GET_NODES_IN_OPEN_CLOSED ((OBJECT BFS)) (
	APPEND  (GET-OPEN OBJECT) (GET-CLOSED OBJECT)
))

; check if the CITY_TO_CHECK which is a city is in open or closed lists in BFS
(DEFMETHOD FIND_IF_IN_OPEN_OR_CLOSED ((OBJECT BFS) CITY_TO_CHECK) (
	COND
	((GET_NODES_IN_OPEN_CLOSED OBJECT) (
		DOLIST (N (GET_NODES_IN_OPEN_CLOSED OBJECT)) (
			IF (STRING-EQUAL N CITY_TO_CHECK) (RETURN-FROM FIND_IF_IN_OPEN_OR_CLOSED T)
		)
	))
	(T NIL)
))

; add parent-children pair into the map, to represent the connected of parent to children, a tree structure in a lists in BFS
; so its like (parent, (children ....))
(DEFMETHOD ADD_TO_MAP ((OBJECT BFS)) (
	SET-MAP (APPEND (GET-MAP OBJECT) (LIST (CONS (GET-CURRENT_CHECK OBJECT) (LIST (GET-TEMP_MAP OBJECT))))) OBJECT
))

; add the current checking city to closed list in BFS
(DEFMETHOD ADD_TO_CLOSED ((OBJECT BFS)) (
	SET-CLOSED (APPEND (GET-CLOSED OBJECT) (LIST (GET-CURRENT_CHECK OBJECT))) OBJECT
))

; add a city to open list in BFS
(DEFMETHOD PUSH_BACK ((OBJECT BFS) CITY) (
	SET-OPEN (APPEND (GET-OPEN OBJECT) (LIST CITY)) OBJECT
))

; remove a city from the queue of open list of BFS
; use it as current checking city
(DEFMETHOD POP_FRONT ((OBJECT BFS)) (
	IF (> (LENGTH (GET-OPEN OBJECT)) 0) (
                     PROGN
			(SET-CURRENT_CHECK (CAR (GET-OPEN OBJECT)) OBJECT)
                        (SET-OPEN (CDR (GET-OPEN OBJECT)) OBJECT)
                        ()
			(RETURN-FROM POP_FRONT T)
	) NIL
))

; this is a function to add current children connected to the current checking city (parent)
(DEFMETHOD INSERT_TEMP_MAP ((OBJECT BFS) CITY) (
	SET-TEMP_MAP (APPEND (GET-TEMP_MAP OBJECT) (LIST CITY)) OBJECT
))

; this method will check the current checking city (parent), and add its child not included in open or closed list to open list and into the children list of this parent
(DEFMETHOD DISCOVER ((OBJECT BFS)) (
	PROGN
	(DOLIST (N (ORDER (GET_ADJACENT_CITIES (GET-CURRENT_CHECK OBJECT)) (GET-ORDER OBJECT))) (
		IF(NOT (FIND_IF_IN_OPEN_OR_CLOSED OBJECT N)) (
			PROGN
				(INSERT_TEMP_MAP OBJECT N)
				(PUSH_BACK OBJECT N)
		)
	))
	(ADD_TO_MAP OBJECT)
	(ADD_TO_CLOSED OBJECT)
	(SET-TEMP_MAP (LIST) OBJECT)
))

; calls an iteration on a city from open list, return true if got city to check, return nil if there is no city left to check
(DEFMETHOD ITERATE ((OBJECT BFS)) (
	IF (POP_FRONT OBJECT) (
		PROGN
			(DISCOVER OBJECT)
            (FORMAT T "~A Closed List : ~S ~%" (GET-NAME OBJECT) (GET-CLOSED OBJECT))
            (FORMAT T "~A Open List : ~S ~%" (GET-NAME OBJECT) (GET-OPEN OBJECT))
            (FORMAT T "~A Parent-Child Map List : ~S ~%" (GET-NAME OBJECT) (GET-MAP OBJECT))
			(RETURN-FROM ITERATE T)
	) NIL
))

; init the states of BFS
(DEFMETHOD INIT ((OBJECT BFS)) (
	PROGN
	(PUSH_BACK OBJECT (GET-START OBJECT))
	(POP_FRONT OBJECT)
	(DISCOVER OBJECT)
))

; check both BFS objects if their open and closed lists has one common city
(DEFMETHOD FIND_SAME_CITY_IN_BOTH_BFS((BEGIN BFS) (END BFS)) (
	PROGN
	(DOLIST (N (GET_NODES_IN_OPEN_CLOSED BEGIN)) (
		IF(FIND_IF_IN_OPEN_OR_CLOSED END N) (
			RETURN-FROM FIND_SAME_CITY_IN_BOTH_BFS N
		)
	))
	NIL
))

; find the parent city by checking its child through the parent-children pair
(DEFMETHOD FIND_PARENT_WITH_CHILD ((OBJECT BFS) CITY) (
        PROGN
        (DOLIST (N (GET-MAP OBJECT)) (
                DOLIST(M (NTH 1 N)) (
                        IF(STRING-EQUAL M CITY) (
				RETURN-FROM FIND_PARENT_WITH_CHILD (CAR N)
                        )
		)
	))
	NIL
))

; trace from the intersected city back to the root and form a route from root to intersected city
(DEFMETHOD TRACE_PATH_FROM_CITY_BACK_TO_ROOT((OBJECT BFS) CITY) (
	LET ((ROUTE (LIST)) (TEMP CITY))
	(LOOP
		(SETQ TEMP (FIND_PARENT_WITH_CHILD OBJECT TEMP))
		(SETQ ROUTE (CONS TEMP ROUTE))
		(WHEN (STRING-EQUAL TEMP (GET-START OBJECT)) (RETURN-FROM TRACE_PATH_FROM_CITY_BACK_TO_ROOT ROUTE))
	)
))

; get route from both BFS objects from starting city to intersected city to goal city
(DEFMETHOD FIND_PATH_FROM_BOTH_BFS((BEGIN BFS) (END BFS) CITY) (
        LET ((ROUTE_START (LIST)) (ROUTE_END (LIST)) (ROUTE_COMPLETE (LIST)))
        (SETQ ROUTE_START (TRACE_PATH_FROM_CITY_BACK_TO_ROOT BEGIN CITY))
        (SETQ ROUTE_END (TRACE_PATH_FROM_CITY_BACK_TO_ROOT END CITY))
        (SETQ ROUTE_END (REVERSE ROUTE_END))
        (SETQ ROUTE_COMPLETE (APPEND ROUTE_START (LIST CITY) ROUTE_END))
	(RETURN-FROM FIND_PATH_FROM_BOTH_BFS ROUTE_COMPLETE)
))

; the culmination of bidirectional search with BFS
(DEFMETHOD BDS_BFS(START GOAL ORDER (OBJ1 BFS) (OBJ2 BFS)) (
	LET ((FOUND_CITY "") (POP_START T) (POP_END T))
	(PROGN
		(SET-START START OBJ1)
		(SET-GOAL GOAL OBJ1)
        (SET-ORDER ORDER OBJ1)
		(INIT OBJ1)

		(SET-START GOAL OBJ2)
		(SET-GOAL START OBJ2)
        (SET-ORDER ORDER OBJ2)
		(INIT OBJ2)

		(LOOP
			(SETQ POP_START (ITERATE OBJ1))
			(SETQ POP_END (ITERATE OBJ2))

			(IF(AND (NOT POP_START) (NOT POP_END)) (
				RETURN-FROM BDS_BFS NIL
			))

			(SETQ FOUND_CITY (FIND_SAME_CITY_IN_BOTH_BFS OBJ1 OBJ2))

			(IF (NOT (NULL FOUND_CITY)) (
                PROGN
                (FORMAT T "Number of Nodes Generated : ~D  ~%" (+ (LENGTH (GET-CLOSED OBJ1)) (LENGTH (GET-OPEN OBJ1)) (LENGTH (GET-CLOSED OBJ2)) (LENGTH (GET-OPEN OBJ2))))
				(RETURN-FROM BDS_BFS (FIND_PATH_FROM_BOTH_BFS OBJ1 OBJ2 FOUND_CITY))
			))
		)
        (RETURN-FROM BDS_BFS NIL)
	)
))

(defun extract-path-from-goal (goal start)
  (setf path-goal nil)
  (setf path-goal (cons goal path-goal))
  (loop
    (setf goal (intern goal))
    (setf goal (get goal 'previous-goal))
    (setf path-goal (cons goal path-goal))
    (if (string-equal goal start) (return))
  )
)


(defun extract-path-from-start (goal start)
  (setf path-start nil)
  (setf path-start (cons goal path-start))
  (loop
    (setf goal (intern goal))
    (setf goal (get goal 'previous-start))
    (setf path-start (cons goal path-start))
    (if (string-equal goal start) (return))
  )
)

(defun bds-dfs (start goal order forbidden-city)
  (setf path-found 'false)
  (setf no-solution-start 'false) ; SET NO SOLUTION FOUND FOR START AS FALSE
  (setf no-solution-goal 'false) ; SET NO SOLUTION FOUND FOR GOAL AS FALSE
  (setf n 0) ; COUNTER TO KEEP TRACK OF NUMBER OF ITERATIONS

  (setf open-start (list start)) ; INITIALIZE START NODE
  (setf closed-start nil)        ; INITIALIZE CLOSED LIST FOR START

  (setf open-goal (list goal)) ; INITIALIZE GOAL NODE
  (setf closed-goal nil)       ; INITIALIZE CLOSED LIST FOR GOAL

  (setf output-start-search-tree nil) ; INITIALIZE LIST TO OUTPUT SEARCH TREE FROM START
  (setf output-goal-search-tree nil)  ; INITIALIZE LIST TO OUTPUT SEARCH TREE FROM GOAL

  ; PRINTING OUT THE INITIAL INIFORMATION BEFORE BIDIRECTIONAL SEARCH USING DEPTH-FIRST-SEARCH
  (format t "~%Closed List for Start after Iteration ~d: ~a" n closed-start)
  (format t "~%Open List for Start after Iteration ~d: ~a" n open-start)
  (format t "~%Closed List for Goal after Iteration ~d: ~a" n closed-goal)
  (format t "~%Open List for Goal after Iteration ~d: ~a" n open-goal)
  (format t "~%Start Search Tree at Iteration ~d: ~a" n output-start-search-tree)
  (format t "~%Goal Search Tree at Iteration ~d: ~a" n output-goal-search-tree)

  (loop
    ;;; DEPTH FIRST SEARCH FROM START

    ; IF OPEN LIST FOR START IS NULL AND STILL NO SOLUTION IS FOUND, RETURN NO SOLUTION
    (if (null open-start) (progn (setf no-solution-start 'true)(return "NO SOLUTION FOUND")))

    ; NEXT CITY TO EXPLORE FROM START
    (setf to-explore-from-start (car open-start))

    ; CITIES THAT ARE WAITING TO BE EXPLORED (THE FRINGE)
    (setf open-start (cdr open-start))

    (dolist (x forbidden-city)
    ; IF THE CITY TO BE EXPLORED IS IN FORBIDDEN LIST
      (if (string-equal to-explore-from-start x)
        ; SKIP THE CITY AND PROCEEED TO NEXT AVAILABLE CITY
        (multiple-value-setq (to-explore-from-start open-start) (values (car open-start) (cdr open-start)))
      )
    )

    ; STORE EXPLORED CITIES INTO CLOSED LIST
    (setf closed-start (cons to-explore-from-start closed-start))

    ; GET THE SUCCESSOR CITIES OF CURRENT EXPLORED CITY
    (setf descendants-start (ORDER (GET_ADJACENT_CITIES to-explore-from-start) order))

    ; CHECK AND FILTER CITIES WHICH ARE EXPLORED
    (setf descendants-start (set-difference descendants-start closed-start :test #'string=))
    (setf descendants-start (reverse descendants-start))

    ; APPEND ALL THE SUCCESSOR CITIES OF CURRENT EXPLORED CITY IN OPEN LIST OF START AND DEDUPLICATE
    (setf open-start (append descendants-start (reverse (set-difference open-start descendants-start :test #'string=))))

    ; ASSIGN EACH NODES TO BE EXPLORED WITH THEIR ANTECEDANT TO KEEP TRACK IF SOLUTION PATH FOUND
    (dolist (x descendants-start)
      (setf (get (intern x) 'previous-start) to-explore-from-start)
    )



    ;;; DEPTH FIRST SEARCH FROM GOAL

    ; IF OPEN LIST FOR GOAL IS NULL AND STILL NO SOLUTION IS FOUND, RETURN NO SOLUTION
    (if (null open-goal) (progn (setf no-solution-goal 'true)(return nil)))

    ; NEXT CITY TO EXPLORE FROM GOAL
    (setf to-explore-from-goal (car open-goal))

    ; CITIES THAT ARE WAITING TO BE EXPLORED (THE FRINGE)
    (setf open-goal (cdr open-goal))

    (dolist (x forbidden-city)
    ; IF THE CITY TO BE EXPLORED IS IN FORBIDDEN LIST
     (if (string-equal to-explore-from-goal x)
      ; SKIP THE CITY AND PROCEEED TO NEXT AVAILABLE CITY
      (multiple-value-setq (to-explore-from-goal open-goal) (values (car open-goal) (cdr open-goal)))
     )
    )

    ; STORE EXPLORED CITIES INTO CLOSED LIST
    (setf closed-goal (cons to-explore-from-goal closed-goal))

    ; GET THE SUCCESSOR CITIES OF CURRENT EXPLORED CITY
    (setf descendants-goal (ORDER (GET_ADJACENT_CITIES to-explore-from-goal) order))

    ; CHECK AND FILTER CITIES WHICH ARE EXPLORED
    (setf descendants-goal (set-difference descendants-goal closed-goal :test #'string=))
    (setf descendants-goal (reverse descendants-goal))

    ; APPEND ALL THE SUCCESSOR CITIES OF CURRENT EXPLORED CITY IN OPEN LIST OF GOAL AND DEDUPLICATE
    (setf open-goal (append descendants-goal (reverse (set-difference open-goal descendants-goal :test #'string=))))

    ; ASSIGN EACH NODES TO BE EXPLORED WITH THEIR ANTECEDANT TO KEEP TRACK IF SOLUTION PATH FOUND
    (dolist (x descendants-goal)
      (setf (get (intern x) 'previous-goal) to-explore-from-goal)
    )

    ; TOO KEEP TRACK OF NUMBER OF ITERATIONS
    (incf n)

    (format t "~%Closed List for Start after Iteration ~d: ~a" n closed-start) ; LIST STORING EXPLORED CITIES FROM START IN CURRENT ITERATION
    (format t "~%Open List for Start after Iteration ~d: ~a" n open-start)     ; LIST STORING CITIES TO BE EXPLORED FROM START (THE FRINGE) IN CURRENT ITERATION
    (format t "~%Closed List for Goal after Iteration ~d: ~a" n closed-goal)   ; LIST STORING EXPLORED CITIES FROM GOAL IN CURRENT ITERATION
    (format t "~%Open List for Goal after Iteration ~d: ~a" n open-goal)       ; LIST STORING CITIES TO BE EXPLORED FROM GOAL (THE FRINGE) IN CURRENT ITERATION

    ; CONSTRUCT THE SEARCH TREE FROM START
    (setf start-search-tree (cons (cons to-explore-from-start (cons descendants-start nil)) nil))
    (setf output-start-search-tree (append start-search-tree output-start-search-tree))
    (format t "~%Start Search Tree at Iteration ~d: ~a" n (reverse output-start-search-tree))

    ; CONSTRUCT THE SEARCH TREE FROM GOAL
    (setf goal-search-tree (cons (cons to-explore-from-goal (cons descendants-goal nil)) nil))
    (setf output-goal-search-tree (append goal-search-tree output-goal-search-tree))
    (format t "~%Goal Search Tree at Iteration ~d: ~a" n (reverse output-goal-search-tree))

    ; IF SEARCH FROM START OR SEARCH FROM GOAL MEET EACH OTHER IN A NODE, GET THE INTERSECTED NODE
    (dolist (city closed-goal)
      (if (string-equal city (find city closed-start :test #'string=))
        (progn (setf to-explore-from-goal city) (setf to-explore-from-start city) (setf path-found 'true))
      )
    )

    (format t "~%")

    ; CONSTRUCT THE SOLUTION PATH FROM START AND GOAL
    (if (equal path-found 'true)
      (progn (extract-path-from-goal to-explore-from-goal goal)
        (extract-path-from-start to-explore-from-start start)
        (return "path found")
      )
    )
  )
)


(DEFUN MAINMENU()
	(SETF BFS_1 (MAKE-INSTANCE `BFS))
	(SETF BFS_2 (MAKE-INSTANCE `BFS))

	(FORMAT T "ENTER ORIGIN CITY NAME:~%") ;; GET NAME OF ORIGIN CITY
	(SETQ ORIGIN_CITY (READ-LINE)) ;; EXAMPLE INPUT: ARAD

	(FORMAT T "ENTER DESTINATION CITY NAME:~%") ;; GET NAME OF DESTINATION CITY
	(SETQ DESTINATION_CITY (READ-LINE)) ;; EXAMPLE INPUT: NEAMT

	(FORMAT T "CHOOSE SORT ORDER: ~%1. ASCENDING ~%2. DESCENDING~%ANSWER: ")  ;; CHOOSE SORT ORDER: ASCENDING OR DESCENDING
	(SETQ SORTORDERCHOOSE (READ-LINE))
	(IF (STRING-EQUAL SORTORDERCHOOSE "1")
		(PROGN
			(SETQ SORTORDERCHOOSE "ASCENDING")
        )
		(PROGN
            (IF (STRING-EQUAL SORTORDERCHOOSE "2")
                (PROGN
                  	(SETQ SORTORDERCHOOSE "DESCENDING")
                )
                (PROGN
                    (FORMAT T "INVALID INPUT. PLEASE TRY AGAIN~%")
                    (MAINMENU)
                )
            )
        )
	)
    (FORMAT T "CHOOSE SEARCH ALGORITHM: ~%1. BREADTH-FIRST SEARCH (BFS) ~%2. DEPTH-FIRST SEARCH (DFS)~%ANSWER: ") ;; CHOOSE SEARCH ALGORITHM: BFS OR DSF
    (SETQ ALGORITHMTYPE (READ-LINE))
    (IF (STRING-EQUAL ALGORITHMTYPE "1")
        (PROGN
			(SETF BFS_1 (MAKE-INSTANCE `BFS :INIT-NAME "HEAD"))
			(SETF BFS_2 (MAKE-INSTANCE `BFS :INIT-NAME "TAIL"))
			(SETF SOLUTION_PATH (BDS_BFS ORIGIN_CITY DESTINATION_CITY SORTORDERCHOOSE BFS_1 BFS_2))
			(FORMAT T "Solution Path : ~S  ~%" SOLUTION_PATH)
			(IF (NOT (NULL SOLUTION_PATH)) (FORMAT T "Path Cost : ~D" (- (LENGTH SOLUTION_PATH) 1)))
        )
        (PROGN
            (IF (STRING-EQUAL ALGORITHMTYPE "2")
                (PROGN
					(setq FORBIDDEN_CITY_LIST (list))
					(FORMAT T "ENTER NAME OF FORBIDDEN CITY (ENTER 'SKIP' TO SKIP THIS):~%") ;; GET FIRST FORBIDDEN CITY
					(SETQ FORBIDDEN_CITY_1 (READ-LINE))
					(IF (STRING-EQUAL FORBIDDEN_CITY_1 "SKIP")
						(PROGN )
						(PROGN
							(setq FORBIDDEN_CITY_LIST (list FORBIDDEN_CITY_1))
							(FORMAT T "ENTER ANOTHER NAME OF FORBIDDEN CITY (ENTER 'SKIP' TO SKIP THIS):~%") ;; GET SECOND FORBIDDEN CITY
							(SETQ FORBIDDEN_CITY_2 (READ-LINE))
							(IF (STRING-EQUAL FORBIDDEN_CITY_2 "SKIP")
								(PROGN )
								(PROGN
									(setq FORBIDDEN_CITY_LIST (list FORBIDDEN_CITY_1 FORBIDDEN_CITY_2))
								)
							)
						)
					)


					(bds-dfs ORIGIN_CITY DESTINATION_CITY SORTORDERCHOOSE FORBIDDEN_CITY_LIST)

					(setf start-nodes-generated (+ (list-length open-start) (list-length closed-start)))
					(setf goal-nodes-generated (+ (list-length open-goal) (list-length closed-goal)))
					(setf total-nodes-generated (+ start-nodes-generated goal-nodes-generated))
					(format t "~%Total Number of Nodes Generated: ~d" total-nodes-generated)

					;ADD IF NO SOLUTION FOUND
					(if (or (equal no-solution-start 'true) (equal no-solution-goal 'true)) (format t "~%Cannot Find Solution")
						(progn
							(setf solution-path (remove-duplicates (append path-start (reverse path-goal)) :test #'string=))
							(format t "~%Solution Path: ~a" solution-path)
              (setf path-cost (- (list-length solution-path) 1))
    					(format t "~%Path Cost: ~d" path-cost)
						)
					)


                )
                (PROGN
                    (FORMAT T "TRY AGAIN~%")
                    (MAINMENU)
                )
            )
        )
    )
)

(MAINMENU)
