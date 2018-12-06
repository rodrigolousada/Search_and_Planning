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
;;;                        Aux Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-initial-state ()
	(return-from create-initial-state (make-trip 
										:local_departure "L1" 
										:local_arrival "L1" 
										:instant_departure 0 
										:instant_arrival 0
									)
	)
)

(defun make-state-copy (state)
	(setf new_state (make-shifts 	
							:trips_list (copy-list (shifts-trips_list state))
							:non_allocated_trips (copy-list (shifts-non_allocated_trips state) )))
	(return-from make-state-copy new_state)
)



;; talvez seja necessario fazer sort

(defun successors (state)
	(setf list_successors (list))
	(setf copy_state (make-state-copy state))

	(setf non_allocated (shifts-non_allocated_trips copy_state))
	(setf trip_non_al (nth 0 non_allocated) )
	(setf state_trip_list (shifts-trips_list copy_state))
	(setf (shifts-non_allocated_trips copy_state) (rest (shifts-non_allocated_trips copy_state)))

	(dotimes (trip_i (length state_trip_list)  ) 
		
			
		(cond 
			( 	(and
					(or 	(and 	(equal (trip-local_departure trip_non_al) (trip-local_arrival (nth 0 (last (nth trip_i state_trip_list)))) )
									(>= (trip-instant_departure trip_non_al) (trip-instant_arrival (nth 0 (last (nth trip_i state_trip_list) )))) ) 
							(and 	(not (equal (trip-local_departure trip_non_al) (trip-local_arrival (nth 0 (last (nth trip_i state_trip_list)))) ))
									(>= (trip-instant_departure trip_non_al) (+ (trip-instant_arrival (nth 0 (last (nth trip_i state_trip_list) ))) 40) ))
					)
					(or 	(or 	(and 	(and 	(equal L1 (trip-local_departure (nth 0 (first (nth trip_i state_trip_list)))) )
													(equal L1 (trip-local_arrival trip_non_al) ))
											(> 480 (- (trip-instant_arrival trip_non_al) (trip-instant_departure (nth 0 (first (nth trip_i state_trip_list)))))) )
									(and 	(and 	(equal L1 (trip-local_departure (nth 0 (first (nth trip_i state_trip_list)))) )
													(not (equal L1 (trip-local_arrival trip_non_al) )))
											(> 480 (- (+ (trip-instant_arrival trip_non_al) 40) (trip-instant_departure (nth 0 (first (nth trip_i state_trip_list)))))))
							)
							(or 	(and 	(and 	(not (equal L1 (trip-local_departure (nth 0 (first (nth trip_i state_trip_list)))) ))
													(equal L1 (trip-local_arrival trip_non_al) ))
											(> 480 (- (trip-instant_arrival trip_non_al) (- (trip-instant_departure (nth 0 (first (nth trip_i state_trip_list)))) 40) )) )
									(and 	(and 	(not (equal L1 (trip-local_departure (nth 0 (first (nth trip_i state_trip_list)))) ))
													(not (equal L1 (trip-local_arrival trip_non_al) )))
											(> 480 (- (+ (trip-instant_arrival trip_non_al) 40) (- (trip-instant_departure (nth 0 (first (nth trip_i state_trip_list)))) 40) )))
							)
					)
				)
				( (setf last_meal (trip-local_departure (nth 0 (first (nth trip_i state_trip_list)))))
					(if (or	(<= (- (trip-instant_arrival trip_non_al) last_meal) 240)
							(<=    (+ (trip-local_arrival (nth 0 (last (nth trip_i state_trip_list)))) 40) (trip-instant_departure trip_non_al))
						)
						(setf new_copy_state (make-state-copy copy_state))
						(setf (nth trip_i (shifts-trips_list new_copy_state)) (append (nth trip_i (shifts-trips_list new_copy_state)) (list trip_non_al)) )
						(setf list_successors (append list_successors (list new_copy_state)))	
					)
				)
			)

			
		)
		
	)
	
	(setf new_copy_state (make-state-copy copy_state))
	(setf (shifts-trips_list new_copy_state) (append (shifts-trips_list new_copy_state) (list (list trip_non_al) )))
	(setf list_successors (append list_successors (list new_copy_state)))
	
	; (return-from successors list_successors)
)

(defun is-goal (state)
	(= 0 (list-length non_allocated_trips)) ;;(shifts-non_allocated_trips state)
)

(defun create-trip-prob (prob)
	(setf list_trips '())
	(print prob)
	(dolist (e  prob)
		(setf list_trips (append list_trips (list (make-trip :local_departure (nth 0 e)
															 :local_arrival (nth 1 e)
															 :instant_departure (nth 2 e)
															 :instant_arrival (nth 3 e)
													))))
	)
	(return-from create-trip-prob list_trips)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun faz-afectacao (problem strategy)
	(let( (max_workhours 480)
		  (initial_local "L1")
		  (meal_break	 40)
		  (max_trip_time 240) ;;max tempo sem pausa
		  (min_duration_shift 360)
		  (result_state nil)
		  (all_shifts (make-shifts :trips_list '() :non_allocated_trips (create-trip-prob problem) ))
		)
		
		(cond ((equal strategy "melhor.abordagem") 
				(setf result_state (melhor-abordagem problem)))
			  ((equal strategy "a*.melhor.heuristica") 
				(setf result_state (a-star-best-heuristic all_shifts)))
			  ((equal strategy "a*.melhor.heuristica.alternativa") 
				(setf result_state (a-star-alternative-heuristic problem)))
			  ((equal strategy "sondagem.iterativa") 
				(setf result_state (sondagem-iterativa problem)))
			  ((equal strategy "ILDS") 
				(setf result_state (ilds problem)))
			  ((equal strategy "abordagem.alternativa")
				(setf result_state (melhor-abordagem problem))
			)
		)
	)
)


;;; melhor-abordagem - Iterative a* and estimate-time-left-part-sequential-part-paralel heuristic
; (defun a-star-best-heuristic problem (problem)
; 	(setf initial_state problem)

; 	(setf initial_prob (cria-problema initial_state
; 							(list #'sucessors)
; 						)
; 	)



; 	; (iterative-search (cria-problema (job-shop-problem-to-job-state problem) 
; 	; 				 		(list #'sucessors)
; 	; 					   	:objectivo? #'objective? 
; 	; 					   	:heuristica #'estimate-time-left-part-sequential-part-paralel
; 	; 					   	:custo #'cost-transition-max-machines
; 	; 					)
; 	; )
; )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (setf gentest (faz-afectacao '( (L2 L1 1 25) (L1 L2 34 60) (L5 L1 408 447) (L1 L1 448 551) (L1 L1 474 465) ) "melhor.abordagem"))
(setf test_state (make-shifts :trips_list '() :non_allocated_trips (create-trip-prob '( (L2 L1 1 25) (L1 L2 34 60) (L5 L1 408 447)))))

(setf test_state_suc (make-shifts :trips_list (list (create-trip-prob '( (L2 L1 1 25)))) :non_allocated_trips (create-trip-prob '(  (L3 L2 65 80) (L5 L1 408 447)))))

