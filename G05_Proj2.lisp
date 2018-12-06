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

;; Instrucoes de leitura para a Sofia:
;;
;;'''''''xxxxxx''''''xxxxxx
;;'''''xxxxxxxxxx''xxxxxxxxxx
;;''''xxxxxxxxxxxxxxxx''''xxxx
;;'''xxxxxxxxxxxxxxxxxx''''xxxx
;;'''xxxxxxxxxxxxxxxxxxx''xxxxx
;;'''xxxxxxxxxxxxxxxxxxx 'xxxxx'
;;'''''xxxxxxxxxxxxxxxxxxxxx
;;''''''''xxxxxxxxxxxxxxxx
;;''''''''''xxxxxxxxxxxxx
;;''''''''''''xxxxxxxx
;;''''''''''''''xxxx
;;
;;		1) Antes demais e fulcral entender que eu gosto muito dela
;;		2) Esperar que tenha dormido bem
;;		3) Admitir que eu estava a ser irritante
;;		4) Compensa-la por isso e esperar que fique feliz com o que fiz xD
;;
;;		Agora relativo ao trabalho
;;		1) Apenas passei para código o que estava na cabeça (que espero que esteja a funcionar)
;;		2) Confere se concordas com o racicionio e/ou se me espalhei em algum lado
;;		3) Para ser mais fácil acompanhar o processo acabei a estar sempre a nomear as coisas (para nao acabar com linhas muito grandes e imperceptiveis)
;;		3.1) Eu sei que fica cheio de linhas de codigo e nao gostas
;;		3.1.1) Mas ao menos usei us setf como gostas <3
;;		4) Traducao para 4 siglas que vais ver nos nomes
;;				ld -> local_departure
;;				la -> local_arrival
;;				id -> instant_departure
;;				ia -> instant_arrival
;;		5) Quando uso setfs para algum objecto comeco por fazer do departure (local e instant) e depois arrival (local e instant)
;;		6) Gosto muito de trabalhar contigo		
;;		7) Desculpa mais uma vez!

(defun successors (state)
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
	
	;;Possibilities of going in the end of an existing shift
	(dotimes (shift_i (length state_trip_list)  )
		;;Existing shift
		(setf this_shift (nth shift_i state_trip_list))
		(print (first this_shift))
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
		(dotimes (trip_i (- (length state_trip_list) 1))
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
	
	;;Possibility of this trip create a new shift
	(setf new_copy_state (make-state-copy copy_state))
	(setf (shifts-trips_list new_copy_state) (append (shifts-trips_list new_copy_state) (list (list trip_non_al) )))
	(setf list_successors (append list_successors (list new_copy_state)))
	
	(return-from successors list_successors)
)

(defun is-goal (state)
	(= 0 (list-length non_allocated_trips)) ;;(shifts-non_allocated_trips state)
)

(defun create-trip-prob (prob)
	;; talvez seja necessario fazer sort do prob
	(setf list_trips '())
	;;(print prob)
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

