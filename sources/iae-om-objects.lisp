;============================================================================
; OM-IAE (ISMM Audio Engine)
; Author: Jean Bresson, Diemo Schwarz
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================

;;=======================================
;; Main class and functions
;;=======================================

(in-package :iae)

(defclass! IAE (om::om-cleanup-mixin om::data-stream)
 ((iaeengine-ptr :accessor iaeengine-ptr :initform nil)
  (sounds :initarg :sounds :accessor sounds :initform nil :documentation "a sound or list of sounds to build the IAE container on")
  (channels :accessor channels :initform 1)
  (max-dur :accessor max-dur :initform 10000)
  (samplerate :accessor samplerate :initform 44100)
  (grains :accessor grains :initform nil)
  (markers :accessor markers :initform nil)
  (pipo-module :accessor pipo-module :initform "basic")
  (descriptors :accessor descriptors :initform nil)
  (desc-tracks :accessor desc-tracks :initform nil)
  ;;; needed for play
  (buffer-player :accessor buffer-player :initform nil)
  )
 (:default-initargs :default-frame-type 'IAE-grain))

(defclass! IAE-grain (om::data-frame)
   ((om::date :accessor om::date :initarg :date :initform 0 :documentation "date/time of the grain")
    (source :accessor source :initarg :source :initform 0 :documentation "source num inside IAE")
    (pos :accessor pos :initarg :pos :initform 0 :documentation "position in source")
    (duration :accessor duration :initarg :duration :initform 100 :documentation "duration of the grain")))

(defclass! IAE-request (om::data-frame)
   ((om::date :accessor om::date :initarg :date :initform 0 :documentation "date/time of the grain")
    (descriptor :accessor descriptor :initarg :descriptor :initform 0 :documentation "the descriptor inside IAE/pipo")
    (value :accessor value :initarg :value :initform 0 :documentation "the value of the descriptor")
    (duration :accessor duration :initarg :duration :initform 100 :documentation "duration of the grain")))

;;; utils to generate random grains / requests
(defun make-IAE-grains (n &key (nsources 1) (maxpos 2500) (durtot 10000) (mindur 100) (maxdur 600))
  (sort 
   (loop for i from 1 to n collect
         (make-instance 'IAE-grain :date (random durtot)
                        :source (random (1- nsources))
                        :pos (random maxpos)
                        :duration (+ mindur (random (- maxdur mindur)))))
   '< :key 'date))

(defun gen-random-requests (n &key (descriptor 0) (minval 100) (maxval 1000) (durtot 10000) (mindur 50) (maxdur 300))
  (sort 
   (loop for i from 1 to n collect
         (make-instance 'IAE-request :date (random durtot)
                        :descriptor descriptor
                        :value (+ minval (random (- maxval minval)))
                        :duration (+ mindur (random (- maxdur mindur)))))
   '< :key 'date))


;;======================================================
;; INITIALIZATION OF THE PIPO MODULE
;; module-name can be: "basic" "ircamdescriptor" "slice:fft"  "...:chop"
;;======================================================
(defmethod iae-init-pipo ((self iae) (module-name string) &optional ircamdescriptors-list)

  (let ((*iae (iaeengine-ptr self)))
    
    (if (= 1 (iae-lib::iae_pipo_create *iae module-name))   
        
        (progn
          
          (when (string-equal module-name "ircamdescriptor")
            (iae-set-pipo-ircamdescriptors self ircamdescriptors-list))
          
          (setf (desc-tracks self) ;;; compute descriptors and collect track indices
                (loop for i from 0 to (1- (length (sounds self))) 
                      collect (iae-lib::iae_pipo_run *iae i)))
          
          (let ((num (iae-lib::iae_get_numdescriptors *iae)))
            (om::om-print-format  "[~D] descriptors list =" (list num) "IAE")
            (setf (descriptors self)
                  (loop for i from 0 to (1- num) 
                        collect (let ((desc-name 
                                       (or (ignore-errors                     
                                             (iae-lib::iae_get_descriptorname *iae i))
                                           (format nil "desc_~D" (1+ i)))))
                                  (om::om-print desc-name))))
            )
          )
 
      (om::om-print "Error initializing PiPo" "IAE"))))



(defparameter *default-ircamdescriptors*
  '("TotalEnergy"
    "FundamentalFrequency"
    "SpectralCentroid"
    "Loudness"
    "Sharpness"
    "Spread"
    "HarmonicEnergy"
    "Inharmonicity"
    "Noisiness"))
    

;;; Set the PiPo ircamdescriptors we want to use (if we use this option with IAE)
(defmethod iae-set-pipo-ircamdescriptors ((self iae) ircamdescriptors-list)
  (let* ((*iae (iaeengine-ptr self))
         (nparams (iae-lib::iae_pipo_param_num *iae))
         (desc-list (or ircamdescriptors-list *default-ircamdescriptors*)))
    
    (loop for param-i from 0 to (1- nparams) do
          (let ((name (iae-lib::iae_pipo_param_get_name *iae param-i)))
            ;(om-print-format "-- ~A = ~A" 
            ;                 (iae-lib::iae_pipo_param_get_description *iae param-i)
            ;                 (iae-lib::iae_pipo_param_get_ *iae param-i)
            ;                 )
            (when (string-equal name "ircamdescriptor.descriptors")
            ;(print (iae-lib::iae_pipo_param_get_type *iae param-i))
            ;(let ((numdesc (iae-lib::iae_pipo_param_enum_get_num *iae name)))
            ;   (loop for d from 0 to (1- numdesc) do
            ;      (print  (iae-lib::iae_pipo_param_enum_get_element *iae name d))))
              (loop for desc in desc-list 
                    for d = 0 then (+ d 1) do
                    (iae-lib::iae_pipo_param_set_string *iae name d desc)
                    )
              )
            ))
    ))


;;;======================================================
;;; READ FROM IAE
;;;======================================================

(defun iae-info (iae)
  (iae-lib::iae_info_get_string (iaeengine-ptr iae) (oa::om-make-null-pointer)))

(defmethod get-sound-descriptors ((self iae) src-index &optional (t1 0) (t2 nil))
  (let* ((*iae (iaeengine-ptr self))
         (numdesc (length (descriptors self)))
         (framedescbuffer (fli::allocate-foreign-object :type :float :nelems numdesc))
         (size (iae-lib::iae_get_track_size *iae src-index (nth src-index (desc-tracks self)))))
    (unwind-protect 
        (let ((curr-time -1))
          (loop for i from 0 to (1- size) 
                while (or (null t2) (<= curr-time t2))
                do (setq curr-time (iae-lib::iae_get_descriptor_data *iae src-index i framedescbuffer))
                when (and (>= curr-time t1) (or (null t2) (<= curr-time t2)))
                collect
                (cons (- curr-time t1)
                      (loop for x from 0 to (1- numdesc) collect 
                            (fli:dereference framedescbuffer :index x :type :float)))))
      (fli:free-foreign-object framedescbuffer))
    ))

;;; Returns a sound buffer with a grain from given pos in IAE
(defmethod iae-synth ((self iae) source pos dur)
  (when (iaeengine-ptr self)
    (let* ((*iae (iaeengine-ptr self))
           (nsamples (ceiling (* dur (iae::samplerate self) 0.001)))
           (omsnd (make-instance 'om::om-internal-sound :n-channels (channels self) :smpl-type :float
                                 :n-samples nsamples :sample-rate 44100))
           (**samples (make-audio-buffer (channels self) nsamples)))

;   Granular = 0,    asynchronous granular synthesis
;   Segmented = 1,   concatenative synthesis (needs at least 1 marker)
;   Synchronous = 2  synchronous granular synthesis (needs at least 2 markers)
      (iae-lib::iae_set_SynthMode *iae 0)

      (when (< source (length (sounds self)))
        (iae-lib::iae_set_sourceindex *iae source))
      (iae-lib::iae_set_Cyclic *iae nil)
      (iae-lib::iae_set_CenteredGrains *iae nil)
      (iae-lib::iae_set_Attack *iae 0.0d0 0.5d0)
      (iae-lib::iae_set_Attack *iae 0.0d0 0.5d0)
      (iae-lib::iae_set_position *iae (coerce pos 'double-float) 0.0d0)
      ;(iae-lib::iae_set_positionvar *iae 1000.0d0)
      (iae-lib::iae_set_period *iae -0.0d0 0.0d0)
      (iae-lib::iae_set_duration *iae (coerce dur 'double-float) 0.0d0)
      (iae-lib::iae_trigger *iae)
      (iae-lib::iae_synth *iae nsamples **samples (channels self))
      (setf (om::buffer omsnd) (om::make-om-sound-buffer-gc :ptr **samples :count 1 :nch (channels self)))
      omsnd)))

;;; Returns a sound buffer with a grain from given set of descriptor values in IAE
(defmethod iae-synth-desc ((self iae) descriptor value dur)
  (when (iaeengine-ptr self)
    (let* ((*iae (iaeengine-ptr self))
           (nsamples (ceiling (* (max dur 500) (iae::samplerate self) 0.001)))
           (omsnd (make-instance 'om::om-internal-sound :n-channels (channels self) :smpl-type :float
                                 :n-samples nsamples :sample-rate 44100))
           (**samples (make-audio-buffer (channels self) nsamples))
           (framedescbuffer (fli::allocate-foreign-object :type :float :nelems (length (descriptors self)))))
      
;   Granular = 0,    asynchronous granular synthesis
;   Segmented = 1,   concatenative synthesis (needs at least 1 marker)
;   Synchronous = 2  synchronous granular synthesis (needs at least 2 markers)
      (iae-lib::iae_set_SynthMode *iae 1)

      (iae-lib::iae_set_Cyclic *iae nil)
      (iae-lib::iae_set_CenteredGrains *iae nil)
      (iae-lib::iae_set_Attack *iae 10.0d0 0.0d0)
      (iae-lib::iae_set_period *iae -0.0d0 0.0d0)
      ;(iae-lib::iae_set_positionvar *iae 0.0d0)
      (iae-lib::iae_set_duration *iae (coerce dur 'double-float) 1.0d0)
      
      (when (descriptors self)
        (let* ((n (length (descriptors self)))
               (vals (make-list n :initial-element 0.0))
               (weights (make-list n :initial-element 0.0)))
          (setf (nth descriptor vals) (float value))
          (setf (nth descriptor weights) 1.0)
          
          (iae-lib::iae_set_target *iae n (cffi::foreign-alloc :float :initial-contents vals))
          (iae-lib::iae_set_weight *iae n (cffi::foreign-alloc :float :initial-contents weights))
          ))
      
      (iae-lib::iae_set_k *iae 3)
      
      (iae-lib::iae_select_new *iae T)
      ;(let* ((selind (iae-lib::iae_get_selectedsegmentindex *iae 0))
      ;       (selbuf (iae-lib::iae_get_SelectedSourceIndex *iae 0))
      ;       (seltime (iae-lib::iae_get_descriptor_data *iae selbuf selind framedescbuffer)))
      ;  (print (format nil "VALUE: ~D found in buffer ~D - index ~D - time=~D" value selbuf selind seltime))
      ;  )
      ;(iae-lib::iae_trigger *iae)
      (iae-lib::iae_synth *iae nsamples **samples (channels self))

      (setf (om::buffer omsnd) (om::make-om-sound-buffer-gc :ptr **samples :count 1 :nch (channels self)))
      omsnd)))


;;;==============================================================================
;;; Connections with OM visual environment
;;;==============================================================================

(in-package :om)

(defmethod additional-class-attributes ((self iae::IAE)) 
  '(iae::channels iae::max-dur iae::grains iae::markers iae::pipo-module))

(defmethod data-stream-frames-slot ((self iae::IAE)) 'iae::grains)

(defmethod play-obj? ((self iae::IAE)) t)

(defmethod get-obj-dur ((self iae::IAE)) (iae::max-dur self))

(defmethod om-cleanup ((self iae::IAE))
  (when (iae::iaeengine-ptr self)
    (om-print (format nil "deleting engine of ~A [~A]" self (iae::iaeengine-ptr self)) "GC")
    (iae-lib::iae_delete (iae::iaeengine-ptr self))
    (when (iae::buffer-player self) (free-buffer-player (iae::buffer-player self)))
  ))

;;; called each time an instance is created
;;; => mostly memory allocations
(defmethod initialize-instance :after ((self iae::IAE) &rest initargs)
  
  (om-print (format nil "Initializing IAE for ~A" self) "IAE")
  
  (let* ((sr (iae::samplerate self))
         (size (round (* (iae::max-dur self) sr) 1000))
         (nch (iae::channels self)))
    
    (setf (iae::iaeengine-ptr self) (iae-lib::iae_new (iae::samplerate self) 512 (iae::channels self) 1))
    
    (set-object-time-window self 100)

    (let ((audio-buffer (fli::allocate-foreign-object 
                         :type :pointer :nelems nch
                         :initial-contents (loop for c from 1 to nch
                                                 collect
                                                 (fli::allocate-foreign-object :type :float :nelems size :initial-element 0.0)))))
  
      (setf (iae::buffer-player self) 
            (make-player-from-buffer audio-buffer size nch sr)))))


;;; called additionally (and later) when an instance is created 
;;; or updated in the visual program execution/manipulations
(defmethod om-init-instance ((self iae::IAE) &optional args)
  
  (setf (iae::sounds self) (list! (iae::sounds self)))
    
  (when (iae::sounds self)
    ;;; can be called several times for the different sources
    (loop for s in (iae::sounds self) when (get-sound-file s) do
          (iae-lib::iae_read (iae::iaeengine-ptr self) (namestring (get-sound-file s)) (cffi-sys::null-pointer)))
    
    ;(iae-lib::iae_set_MarkerTrackSdif (iaeengine-ptr self) -1 "XCUD" "XCUD")
    ;(iae-lib::iae_set_DescriptorTrackSdif (iaeengine-ptr self) -1 "XCUD" "XCUD")
   
    
    ;(iae::init-pipo self "basic")     ;;  options: "basic" "ircamdescriptor" "slice:fft"  "...:chop"
    (if (listp (iae::pipo-module self))
        (iae::iae-init-pipo self "ircamdescriptor" (iae::pipo-module self))
      (iae::iae-init-pipo self (iae::pipo-module self)))

    (om-print (iae::iae-info self) "IAE")
  
    (iae-lib::iae_update_kdtree (iae::iaeengine-ptr self) T)
    (om-print "KDTree updated." "IAE")
    (iae-lib::iae_set_SynthMode (iae::iaeengine-ptr self) 1)
    (om-print "IAE engine ready!" "IAE"))
    
  self)

;; (om-init-instance (make-instance 'iae::iae))
; (gc-all)

;;;=========================
;;; DISPLAY
;;;=========================

;;; do that better with IDs etc.
(defmethod get-cache-display-for-draw ((self iae::IAE))
  (append (call-next-method)
          (list (iae::iae-info self))))


(defmethod draw-mini-view ((self iae::IAE) (box t) x y w h &optional time)
  (call-next-method)
  (let ((display-cache (get-display-draw box)))
    (om-with-font 
     (om-def-font :font1 :size 8) 
     (loop for str in (string-lines-to-list (car display-cache))
           for y = 16 then (+ y 10) do 
           (om-draw-string 10 y str))
     )))


;;;============================================
;;; DATA-STREAM ACTIONS FOR IAE
;;;============================================

(defmethod item-duration ((self iae::IAE-grain)) (iae::duration self))

(defmethod data-frame-text-description ((self iae::IAE-grain)) `("IAE GRAIN:" ,(format nil "~D in source ~A" (iae::pos self) (iae::source self))))
(defmethod data-frame-text-description ((self iae::IAE-request)) `("IAE REQUEST:" ,(format nil "desc. ~A = ~D" (iae::descriptor self) (iae::value self))))

(defmethod y-range-for-object ((self iae::IAE)) '(-1000 3000))

(defmethod get-frame-color ((self iae::IAE-grain)) (om-make-color-alpha (get-midi-channel-color (1+ (iae::source self))) 0.5))
(defmethod get-frame-posy ((self iae::IAE-grain)) (+ 50 (iae::pos self)))
(defmethod get-frame-sizey ((self iae::IAE-grain)) 
  (or (getf (attributes self) :posy)
      (setf (getf (attributes self) :posy) (+ 200 (om-random -50 50)))))

(defmethod get-frame-color ((self iae::IAE-request)) (om-make-color-alpha (get-midi-channel-color (1+ (iae::descriptor self))) 0.5))
(defmethod get-frame-posy ((self iae::IAE-request)) (iae::value self))
(defmethod get-frame-sizey ((self iae::IAE-request)) 
  (or (getf (attributes self) :posy)
      (setf (getf (attributes self) :posy) (+ 200 (om-random -50 50)))))


;;;===============================
;;; AUDIO RENDERING
;;;===============================

(defun iae-add-grain (iae audiobuffer dur at)
  (when (iae::buffer-player iae)
    (let ((pos (round (* at (iae::samplerate iae)) 1000))
          (size (round (* dur (iae::samplerate iae)) 1000)))
      (dotimes (c (iae::channels iae))
        (dotimes (i size)
          (unless (>= i (bp-size (iae::buffer-player iae)))
            (setf (fli:dereference 
                   (fli:dereference (bp-buffer (iae::buffer-player iae)) :index c :type :pointer)
                   :index (+ pos i) :type :float)
                  (+ (fli:dereference 
                      (fli:dereference (bp-buffer (iae::buffer-player iae)) :index c :type :pointer) 
                      :index (+ pos i) :type :float)
                     (fli:dereference 
                      (fli:dereference (om-sound-buffer-ptr (om::buffer audiobuffer)) :index c :type :pointer)
                      :index i :type :float)))
            ))))))

(defmethod make-grain-from-frame ((self iae::IAE) (frame iae::IAE-grain))
  (iae::iae-synth self (source frame) (iae::pos frame) (iae::duration frame)))

(defmethod make-grain-from-frame ((self iae::IAE) (frame iae::IAE-request))
  (iae::iae-synth-desc self (iae::descriptor frame) (iae::value frame) (iae::duration frame)))


;;; This is the action performed when we "play" an IAE object
(defmethod get-computation-list-for-play ((object iae::IAE) &optional interval)
  (loop for frame in (remove-if #'(lambda (date) (or (< date (car interval)) (>= date (cadr interval))))
                                (data-stream-get-frames object) 
                                :key 'date)
        do (iae-add-grain object
                          (make-grain-from-frame object frame)
                          (duration frame) (date frame))
        )
  nil)

#|
(defmethod get-computation-list-for-play ((object iae) &optional interval)
  (mapcar 
   #'(lambda (frame) 
       (list (- (date frame) (time-window object))
             (date frame)
             #'(lambda ()
                 (iae-add-grain object
                                (make-grain-from-frame object frame)
                                (duration frame) (date frame)))))
   (remove-if #'(lambda (date) (or (< date (car interval)) (>= date (cadr interval))))
              (data-stream-get-frames object) 
              :key 'date)))
|#


(defmethod iae-reset ((self iae::IAE))
  (when (iae::buffer-player self)
    (dotimes (c (iae::channels self))
      (dotimes (i (bp-size (iae::buffer-player self)))
        (setf (fli:dereference 
               (fli:dereference (bp-buffer (iae::buffer-player self)) :index c :type :pointer) 
               :index i :type :float)
              0.0)))))

(defmethod get-action-list-for-play ((object iae::IAE) interval &optional parent)
  (external-player-actions object interval parent))


(defmethod player-play-object ((self scheduler) (object iae::IAE) caller &key parent interval)
  (start-buffer-player (iae::buffer-player object) 
                       :start-frame (if (car interval)
                                        (round (* (car interval) (/ (iae::samplerate object) 1000.0)))
                                      (or (car interval) 0)))
  (call-next-method))

(defmethod player-stop-object ((self scheduler) (object iae::IAE))
  (stop-buffer-player (iae::buffer-player object))
  (iae-reset object)
  (call-next-method))

(defmethod player-pause-object ((self scheduler) (object iae::IAE))
  (pause-buffer-player (iae::buffer-player object))
  (call-next-method))

(defmethod player-continue-object ((self scheduler) (object iae::IAE))
  (continue-buffer-player (iae::buffer-player object))
  (call-next-method))

(defmethod set-object-time ((self iae::IAE) time) 
  (iae-reset self)
  (jump-to-time (iae::buffer-player self) time)
  (call-next-method))


