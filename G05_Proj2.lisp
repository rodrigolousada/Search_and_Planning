;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      PP Project
;;;                   	   Group 5
;;;   81105 - Sofia Aparicio and 81115 - Rodrigo Lousada
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user) 
(setf *random-state* (make-random-state t))
(compile-file "procura.lisp")
(load "procura")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        Global Var
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct trip
	local_departure
	local_arrival
	instant_departure
	instant_arrival
)

(defstruct shifts
	trips_list
	non_allocated_trips
	cost
	; max.workhours 	;8horas
	; initial.local 	;L1
	; meal.break		;40min
	; max.trip.time	; 4hours sem pausa
	;necessario contar o tempo que o trabalhador leva ate chegar ao local de inicio a partir de L1
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                Structure auxiliar Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates a fake initial state with L1 and 0
(defun create-initial-state ()
	(return-from create-initial-state (make-trip 
										:local_departure "L1" 
										:local_arrival "L1" 
										:instant_departure 0 
										:instant_arrival 0
									)
	)
)

;; Sorts list of trips by instant of departure
( defun sort-list (problem)
	(return-from sort-list (sort problem #'< :key #'trip-instant_departure))
)

;; Turn trips gave in the problem into instances of structure trip
(defun create-trip-prob (prob)
	(setf list_trips '())
	
	(dolist (e  prob)
		(setf list_trips (append list_trips (list (make-trip :local_departure (nth 0 e)
															 :local_arrival (nth 1 e)
															 :instant_departure (nth 2 e)
															 :instant_arrival (nth 3 e)
													))))
	)
	(return-from create-trip-prob (sort-list list_trips))
)

;; Makes deep copy of our structure shifts
(defun make-state-copy (state)
	(setf new_state (make-shifts 	
							:trips_list (copy-list (shifts-trips_list state))
							:non_allocated_trips (copy-list (shifts-non_allocated_trips state) )))
	(return-from make-state-copy new_state)
)

;; Check if two trips are the same
(defun trip-is-equal (trip1 trip2)
	(return-from trip-is-equal (equalp trip1 trip2))
)
						
;; Check if two states are equal
(defun state-is-equal (state1 state2)
	(setf s1_trip_list (shifts-trips_list state1))
	(setf s2_trip_list (shifts-trips_list state2))
	(setf s1_non_alloc_trips (shifts-non_allocated_trips state1))
	(setf s2_non_alloc_trips (shifts-non_allocated_trips state2))
	
	;;if arrays are not the same length
	(cond ((or 	(not (equal (length s1_trip_list) (length s2_trip_list)))
				(not (equal (length s1_non_alloc_trips) (length s2_non_alloc_trips))))
			(return-from state-is-equal nil))
	)
	
	;;If list non alloc is not equal
	(dotimes (i (length s1_non_alloc_trips))
		(cond ((not (trip-is-equal (nth i s1_non_alloc_trips) (nth i s2_non_alloc_trips)))
			(return-from state-is-equal nil))
		)
	)
	
	;; Go through every shift
	(dotimes (shift_index (length s1_trip_list))
		;; Check if shift sizes is the same
		(cond ((not (equal (length (nth shift_index s1_trip_list)) (length (nth shift_index s2_trip_list))))
			(return-from state-is-equal nil))
		)
		;; Check if shift trips are the same
		(dotimes (i (length (nth shift_index s1_trip_list)))
			(cond ((not (trip-is-equal (nth i (nth shift_index s1_trip_list)) (nth i (nth shift_index s2_trip_list))))
				(return-from state-is-equal nil))
			)
		)
	)
	(return-from state-is-equal T)
)


;; Translates final state from strcuture shift into list of lists
(defun translate_solution (solution_struct)
	(setf solution_list (list))
	(setf shifts_list (shifts-trips_list solution_struct))
	(dolist (shift shifts_list)
			(setf shift_list (list))
			(dolist (trip_struct shift)
				(setf trip_list (list 	(trip-local_departure trip_struct)
										(trip-local_arrival trip_struct)
										(trip-instant_departure trip_struct)
										(trip-instant_arrival trip_struct)))
				(setf shift_list (append shift_list (list trip_list)))
			)
			(setf solution_list (append solution_list (list shift_list)))
	)
	(return-from  translate_solution solution_list)
)

(defun module (number)
	(return-from module (sqrt (* number number)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            		    Cost Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; goal is to minimize the cost

(defun cost-size-shifts (state)
	;;(format t "entrou no cost~%")
	(setf cost 0)
	(dolist (shift (shifts-trips_list state))
		(cond ((not (equal 'L1 (trip-local_departure (first shift))))
				(setf cost (+ cost 40))))
		(cond ((not(equal 'L1 (trip-local_arrival (nth 0 (last shift)))))
				(setf cost (+ cost 40))))
		(setf cost (+ cost (- (trip-instant_arrival (nth 0 (last shift))) (trip-instant_departure (first shift)))))
	)
	(return-from cost-size-shifts cost )
	;;(format t "saiu do cost~%")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     Auxiliar Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;	ld -> local_departure
;;	la -> local_arrival
;;	id -> instant_departure
;;	ia -> instant_arrival

(defun successors (state)
	;;(format t "Got inside sucessores~%")
	(setf list_successors (list))
	(setf copy_state (make-state-copy state))

	(setf non_allocated (shifts-non_allocated_trips copy_state))
	(setf trip_non_al (nth 0 non_allocated) )
	(setf state_trip_list (shifts-trips_list copy_state))
	(setf (shifts-non_allocated_trips copy_state) (rest (shifts-non_allocated_trips copy_state)))
	
	;;Constants
	(setf meal_treshold 240)
	(setf shift_treshold 480)
	(setf penalty 40)
	(setf meal_time 40)

	;;New to allocate
	(setf new_ld (trip-local_departure trip_non_al))
	(setf new_la (trip-local_arrival trip_non_al))
	(setf new_id (trip-instant_departure trip_non_al))
	(setf new_ia (trip-instant_arrival trip_non_al))
	
	
	(if (equal 'L1 new_ld)
				(setf new_start_penalty 0)
				(setf new_start_penalty penalty))
	(if (equal 'L1 new_la)
				(setf new_end_penalty 0)
				(setf new_end_penalty penalty))
	
	(setf new_id_if_first (+ new_id new_start_penalty))	
	(setf new_ia_if_last (+ new_ia new_end_penalty))
	
	;;(format t "New is ready~%")
	
	;;Possibilities of going in the end of an existing shift
	(dotimes (shift_i (length state_trip_list)  )
		;;Existing shift
		(setf this_shift (nth shift_i state_trip_list))
		;;(print (first this_shift))
		(setf shift_ld (trip-local_departure (first this_shift)))
		(if (equal 'L1 shift_ld)
				(setf start_penalty 0)
				(setf start_penalty penalty))
		(setf shift_id (- (trip-instant_departure (first this_shift)) start_penalty))
		
		(setf shift_la (trip-local_arrival (nth 0 (last this_shift))))
		(setf shift_ia (trip-instant_arrival (nth 0 (last this_shift))))
		(if (equal shift_la new_ld)
				(setf change_station_penalty 0)
				(setf change_station_penalty penalty))
		
		;;Get Last meal time
		(setf last_meal shift_id)
		(dotimes (trip_i (- (length this_shift) 1))
			(setf this_trip (nth trip_i this_shift))
			(setf next_trip (nth (+ trip_i 1) this_shift))
			(setf this_ia (trip-instant_arrival this_trip))
			(setf next_id (trip-instant_departure next_trip))
			(setf break_between (- next_id this_ia))
			(cond ((>= break_between meal_time) (setf last_meal next_id)))
		)
		
		(cond 	(
				(and 	
					(and	(>= new_id (+ shift_ia change_station_penalty))
							(>= shift_treshold (- new_ia_if_last shift_id))
					)
					(or 	(>= meal_treshold (- new_ia last_meal))
							(>= new_id (+ shift_ia meal_treshold))
					)
				)
					(setf new_copy_state (make-state-copy copy_state))
					(setf (nth shift_i (shifts-trips_list new_copy_state)) (append (nth shift_i (shifts-trips_list new_copy_state)) (list trip_non_al)) )
					(setf list_successors (append list_successors (list new_copy_state)))
				)
		)	
	)
	
	;;(format t "no more available shifts, lets create on for us~%")
	
	;;Possibility of this trip create a new shift
	(setf new_copy_state (make-state-copy copy_state))
	(setf (shifts-trips_list new_copy_state) (append (shifts-trips_list new_copy_state) (list (list trip_non_al) )))
	(setf list_successors (append list_successors (list new_copy_state)))
	
	;;(format t "Getting out of successores with: ~%")
	;;(dolist (successor list_successors)
	;;	(print (translate_solution successor))
	;;)
	;;(format t "It's ouuuuuttt~%")
	(return-from successors list_successors)
)

(defun is-goal (state)
	(= 0 (length (shifts-non_allocated_trips state)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     Heuristic functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun h-a-star-best-heuristic (state)
 	(if (is-goal state) (return-from a-star-heuristic 0))
)

(defun cost-shift-quant (state)
	(setf quant_shifts (length (shifts-trips_list state)))
	(return-from cost-shift-quant (* quant_shifts 10))
)

(defun cost-before-six (state)
	(setf cost 0)
	(dolist (listshift (shifts-trips_list state))
		(if (> 360 (trip-instant_arrival (nth 0 (last listshift))))
			(setf cost (+ cost  (- 360 (trip-instant_arrival (nth 0 (last listshift))) )))
		)
	)
	(return-from cost-before-six cost )
)

(defun cost-time-wasted (state)
	(setf inc_waste 0)
	(dolist (listshift (shifts-trips_list state))
		
		(dotimes (shift_index (length listshift))
					
					(setf flag 0)
					(cond 	( 	(equal shift_index 0)
									(print "inicial")
									(if (not (equal 'L1 (trip-instant_departure (nth shift_index listshift)) ))
										(progn 
											(setf flag 40)
											(if (not (equal nil (nth (+ shift_index 1) listshift)))
												(progn (print "meio")
												(setf inc_waste (+ inc_waste (- (trip-instant_departure (nth (+ shift_index 1) listshift)) 
													(trip-instant_arrival (nth shift_index listshift)) ) )))
											)
										)
									)
							)

							( 	(not (equal nil (nth (+ shift_index 1) listshift))) 
								(print  "meio")
								(setf inc_waste (+ inc_waste (- (trip-instant_departure (nth (+ shift_index 1) listshift)) 
										(trip-instant_arrival (nth shift_index listshift)) ) ))

							)
							( (equal shift_index (- (list-length listshift) 1))
								(print "final")
								(if
									(> 360  (trip-instant_arrival (nth shift_index listshift)))
										(cond 
											( 	(not (equal 'L1 (trip-instant_arrival (nth shift_index listshift)) ))
												(print "entrou nao L1")
												(if (< (+ flag 40) (- 360 (trip-instant_arrival (nth shift_index listshift)) ))
													(progn (print "add")
													(setf inc_waste (+  inc_waste  (- 360 (trip-instant_arrival (nth shift_index listshift)) ) )  ))
													(setf inc_waste (+  inc_waste  (+ flag 40) )  )
												)
											)
											( 	(equal 'L1 (trip-instant_arrival (nth shift_index listshift)) )
												(if (< flag (- 360 (trip-instant_arrival (nth shift_index listshift)) ))
													(setf inc_waste (+  inc_waste  (- 360 (trip-instant_arrival (nth shift_index listshift)) ) )  )
													(setf inc_waste (+  inc_waste  flag )  )
												)
											)
										)
										

									(cond
										( 	(not (equal 'L1 (trip-instant_arrival (nth shift_index listshift)) ))
											(print "entrou bonito")
											(setf inc_waste (+ (+ inc_waste 40) flag) )
										)
										( 	(equal 'L1 (trip-instant_arrival (nth shift_index listshift)) )
											(setf inc_waste (+ inc_waste flag))
										)
									)
								)
							)

					)						
		)
	)
	(return-from cost-time-wasted inc_waste)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     Algorithm functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defun melhor-abordagem (state)
;;)

(defun a-star-best-heuristic (state)
	(setf int-prob  (cria-problema state 
									(list #'successors) 
									:objectivo? #'is-goal 
									:estado= #'state-is-equal 
									:heuristica #'cost-shift-quant))
	(setf best-state (last (first (procura int-prob "a*" :espaco-em-arvore? t))))
	(return-from a-star-best-heuristic (nth 0 best-state))
)

;;(defun a-star-alterantive-heuristic (state)
;;)

;;(defun sondagem-iterativa (state)
;;)

;;(defun ilds (state)
;;)

;;(defun abordagem-alternativa (state)
;;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     Solution function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun faz-afectacao (problem strategy)
	(let( (max_workhours 480)
		  (initial_local "L1")
		  (meal_break	 40)
		  (max_trip_time 240) ;;max tempo sem pausa
		  (min_duration_shift 360)
		  (result_state nil)
		  (root_state (make-shifts :trips_list '() :non_allocated_trips (create-trip-prob problem) ))
		)
		
		(cond ((equal strategy "melhor.abordagem") 
				(setf result_state (melhor-abordagem root_state)))
			  ((equal strategy "a*.melhor.heuristica") 
				(setf result_state (a-star-best-heuristic root_state)))
			  ((equal strategy "a*.melhor.heuristica.alternativa") 
				(setf result_state (a-star-alternative-heuristic root_state)))
			  ((equal strategy "sondagem.iterativa") 
				(setf result_state (sondagem-iterativa root_state)))
			  ((equal strategy "ILDS") 
				(setf result_state (ilds root_state)))
			  ((equal strategy "abordagem.alternativa")
				(setf result_state (abordagem-alternativa root_state))
			)
		)
		(return-from faz-afectacao (translate_solution result_state))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf gentest (faz-afectacao '( (L2 L1 1 25) (L1 L2 34 60) (L5 L1 408 447) (L1 L1 448 551) (L1 L1 474 465) ) "a*.melhor.heuristica"))
(setf test_state (make-shifts :trips_list '() :non_allocated_trips (create-trip-prob '( (L2 L1 1 25) (L1 L2 34 60) (L5 L1 408 447)))))

(setf test_state_suc_1 (make-shifts :trips_list (list (create-trip-prob '( (L2 L1 1 25)))) :non_allocated_trips (create-trip-prob '(  (L3 L2 65 80) (L5 L1 408 447)))))

(setf test_state_suc_2 (make-shifts :trips_list (list (create-trip-prob '( (L2 L1 1 25)))) :non_allocated_trips (create-trip-prob '(  (L3 L2 65 80) (L5 L1 408 447)))))

; (setf test_state_suc_3 (make-shifts :trips_list (list (create-trip-prob '( (L2 L1 1 25)))) :non_allocated_trips (create-trip-prob '(  (L3 L2 65 80) (L5 L2 408 447)))))
; (setf test_suc (make-shifts :trips_list (list (create-trip-prob '( (L2 L3 1 100) (L4 L1 190 200) )  ) ) :non_allocated_trips (create-trip-prob '( (L5 L6 300 480)))) )

; (setf test_sort  (create-trip-prob '((L1 L4 100 300) (L4 L3 10 40) (L5 L8 30 100) (L6 L6 20 40)))) 

; (setf test_cost_time_waste (cost-time-wasted (make-shifts 	:trips_list (list (create-trip-prob '( (L2 L3 1 100) (L4 L1 190 200) )  ) ) 
; 															:non_allocated_trips '() )))
(setf test_cost_size_shifts (cost-size-shifts (make-shifts 	:trips_list (list (create-trip-prob '( (L2 L3 1 100) (L4 L2 190 200) )  ) ) 
 															:non_allocated_trips '() )))
; (setf test_cost_before_six (cost-before-six (make-shifts 	:trips_list (list (create-trip-prob '( (L2 L3 1 100) (L4 L1 190 200) )  ) ) 
;															:non_allocated_trips '() )))

(setf test_a_star (translate_solution (a-star-best-heuristic test_state)))