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

;;;==================================
;;; DESCRIPTORS
;;;==================================

(defvar *all-ircam-descriptiors*
  '("SignalZeroCrossingRate"
    "TotalEnergy"
    "SpectralCentroid"
    "SpectralSpread"
    "SpectralSkewness"
    "SpectralKurtosis"
    "SpectralRolloff"
    "SpectralVariation"
    "SpectralDecrease"
    "Loudness"
    "RelativeSpecificLoudness"
    "PerceptualTristimulus"
    "PerceptualOddToEvenRatio"
    "Sharpness"
    "Spread"
    "SpectralFlatness"
    "SpectralCrest"
    "SpectralSlope"
    "Chroma"
    "MFCC"
    "PerceptualSpectralDeviation"
    "PerceptualSpectralCentroid"
    "PerceptualSpectralSpread"
    "PerceptualSpectralSkewness"
    "PerceptualSpectralKurtosis"
    "PerceptualSpectralRolloff"
    "PerceptualSpectralVariation"
    "PerceptualSpectralDecrease"
    "PerceptualSpectralSlope"
    "FundamentalFrequency"
    "Inharmonicity"
    "HarmonicEnergy"
    "NoiseEnergy"
    "Noisiness"
    "HarmonicTristimulus"
    "HarmonicOddToEvenRatio"
    "HarmonicSpectralDeviation"
    "HarmonicSpectralCentroid"
    "HarmonicSpectralSpread"
    "HarmonicSpectralSkewness"
    "HarmonicSpectralKurtosis"
    "HarmonicSpectralVariation"
    "HarmonicSpectralDecrease"
    "HarmonicSpectralSlope"
    "HarmonicSpectralRolloff"))

;;; a reduced list of descriptors that is computed by default
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


(om::defmethod! ircam-descriptors-names ()
  :doc "Returns the list of all available IrcamDescriptors.

Use items of this list to instancitate the :pipo-module attribute of IAE." 
  *all-ircam-descriptiors*)


;;;==================================
;;; IAE
;;;==================================

(defclass! IAE (om::om-cleanup-mixin om::data-stream)
 ((iaeengine-ptr :accessor iaeengine-ptr :initform nil)
  (sounds :initarg :sounds :accessor sounds :initform nil :documentation "a sound or list of sounds to build the IAE container on")
  (channels :accessor channels :initform 1 :documentation "number of channels for audio output")
  (max-dur :accessor max-dur :initform 10000 :documentation "max duration fo the audio output buffer [ms]")
  (samplerate :accessor samplerate :initform 44100 :documentation "sample rate for audio output")
  (grains :accessor grains :initform nil :documentation "a list of timed-requests for granular synthesis")
  ; (markers :accessor markers :initform nil :documentation "markers in audio buffers")
  (pipo-module :accessor pipo-module :initform "descr" :documentation "name of a pipo module for sound analysis") 
  (descriptors :accessor descriptors :initform nil)
  (desc-tracks :accessor desc-tracks :initform nil)
  (chop :accessor chop :initform nil :documentation "chop size for pipo segmentation [number of samples]")
  ;;; needed for play
  (buffer-player :accessor buffer-player :initform nil)
  )
 (:default-initargs :default-frame-type 'IAE-grain)
 (:documentation "IAE is a multi-track container for sounds and sound descriptions are stored data. 

At the same time it is a container for granular synthesis events that are computed dynamically from the audio buffers.

Tracks can be computed and segmented using 'pipo' modules: \"desc\" \"ircamdescriptor\" \"slice:fft\" \"mfcc\" \"<desc,mfcc>\" ... ") 
)


(defmethod om::om-cleanup ((self iae::IAE))
  (when (iae::iaeengine-ptr self)
    (om::om-print-dbg "deleting engine of ~A [~A]" (list self (iae::iaeengine-ptr self)) "GC")
    (iae-lib::iae_delete (iae::iaeengine-ptr self))
    (when (iae::buffer-player self) (om::free-buffer-player (iae::buffer-player self)))
  ))

;;; called each time an instance is created
;;; => mostly memory allocations
(defmethod initialize-instance :after ((self iae::IAE) &rest initargs)
  
  (om::om-print-dbg "Initializing IAE for ~A" (list self) "IAE")
  
  (let* ((sr (iae::samplerate self))
         (size (round (* (iae::max-dur self) sr) 1000))
         (nch (iae::channels self)))
    
    (setf (iae::iaeengine-ptr self) (iae-lib::iae_new  (iae::samplerate self) 512 nch 1))
    
    (om::set-object-time-window self 100)

    (let ((audio-buffer (fli::allocate-foreign-object  
                         :type :pointer :nelems nch
                         :initial-contents (loop for c from 1 to nch
                                                 collect
                                                 (fli::allocate-foreign-object :type :float :nelems size :initial-element 0.0)))))
  
      (setf (iae::buffer-player self) 
            (om::make-player-from-buffer audio-buffer size nch sr))
      )))


;;======================================================
;; INITIALIZATION OF THE PIPO MODULE
;; module-name can be: "desc" "ircamdescriptor" "slice:fft" "mfcc" "<desc,mfcc>" "...:chop"
;;======================================================

(defmethod iae-init-pipo ((self iae))

  (let* ((*iae (iaeengine-ptr self))
         (main-pipo (if (listp (iae::pipo-module self)) "ircamdescriptor" (iae::pipo-module self)))
         (pipo-string (if (chop self) (concatenate 'string main-pipo ":chop") main-pipo)))

    (if (= 1 (iae-lib::iae_pipo_create *iae pipo-string))  ;;;  ; "basic" "ircamdescriptor" "mfcc" "slice:fft"  "...:chop"
        
        (let ()
          
          (when (string-equal main-pipo "ircamdescriptor")
            
            ;;; Set the PiPo ircamdescriptors we want to use (if we use this option with IAE)
            (let ((nparams (iae-lib::iae_pipo_param_num *iae))
                  (desc-list (if (listp (iae::pipo-module self)) (iae::pipo-module self) *default-ircamdescriptors*)))
    
              (loop for param-i from 0 to (1- nparams) do
                    (let ((name (iae-lib::iae_pipo_param_get_name *iae param-i)))
                      ;(om::om-print-dbg  "-- ~A (~A) = ~A" 
                      ;                   (list (iae-lib::iae_pipo_param_get_description *iae param-i)
                      ;                         (iae-lib::iae_pipo_param_get_type *iae param-i)
                      ;                         (iae-lib::iae_pipo_param_get_ *iae param-i))
                      ;                   "OM-IAE")
                      (when (string-equal name "ircamdescriptor.descriptors")
            
            ;(let ((numdesc (iae-lib::iae_pipo_param_enum_get_num *iae name)))
            ;   (loop for d from 0 to (1- numdesc) do
            ;      (print  (iae-lib::iae_pipo_param_enum_get_element *iae name d))))
                        (loop for desc in desc-list 
                              for d = 0 then (+ d 1) do
                              (iae-lib::iae_pipo_param_set_string *iae name d desc)
                              )
                        )
                      ))
              ))    ;;;  END SPECIFIC IrcamDescriptor SECTION
          
          
          ;(iae-lib::iae_pipo_param_set_int *iae "mvavrg.size" 0 10)
          
          (iae-lib::iae_pipo_param_set_int *iae "descr.hopsize" 0 512)
         ; (iae-lib::iae_pipo_param_set_int *iae "ircamdescriptor.hopsize" 0 512)
          (iae-lib::iae_pipo_param_set_int *iae "mfcc.hopsize" 0 512)
          
          (when (chop self)
            (iae-lib::iae_pipo_param_set_int *iae "chop.size" 0 (chop self)))
          
          (setf (desc-tracks self) ;;; compute descriptors and collect track indices
                (loop for i from 0 to (1- (length (sounds self))) 
                      collect (iae-lib::iae_pipo_run *iae i)))
          
          (let ((num (iae-lib::iae_get_numdescriptors *iae)))
            (om::om-print-dbg  "[~D] descriptors" (list num) "IAE")
            (setf (descriptors self)
                  (loop for i from 0 to (1- num) 
                        collect (let ((desc-name 
                                       (or (ignore-errors                     
                                             (iae-lib::iae_get_descriptorname *iae i))
                                           (format nil "desc_~D" (1+ i)))))
                                  ;(om::om-print desc-name)
                                  desc-name
                                  )))
            )
          )
 
      (om::om-print "Error initializing PiPo" "IAE"))
    ))


;;; called additionally (and later) when an instance is created 
;;; or updated in the visual program execution/manipulations
(defmethod om::om-init-instance ((self iae::IAE) &optional args)
  
  (setf (iae::sounds self) (om::list! (iae::sounds self)))
    
  (when (iae::sounds self)
    
    ;;; can be called several times for the different sources
    (loop for s in (iae::sounds self) when (om::get-sound-file s) do
          (iae-lib::iae_read (iae::iaeengine-ptr self) (namestring (om::get-sound-file s)) (cffi-sys::null-pointer)))
    
    ;(iae-lib::iae_set_MarkerTrackSdif (iaeengine-ptr self) -1 "XCUD" "XCUD")
    ;(iae-lib::iae_set_DescriptorTrackSdif (iaeengine-ptr self) -1 "XCUD" "XCUD")
   
    (iae-init-pipo self)     
    
    (om::om-print (iae::iae-info self) "IAE")
  
    (iae-lib::iae_update_kdtree (iae::iaeengine-ptr self) T)
    (om::om-print-dbg "KDTree updated." nil "IAE")
    (iae-lib::iae_set_SynthMode (iae::iaeengine-ptr self) 1)
    (om::om-print-dbg "IAE engine ready!" nil "IAE"))
    
  self)


; (gc-all)


;;;==============================================================================
;;; Connections with OM visual environment
;;;==============================================================================

(defmethod om::additional-class-attributes ((self iae::IAE)) 
  '(iae::channels iae::max-dur iae::grains iae::pipo-module iae::chop))

(defmethod om::data-stream-frames-slot ((self iae::IAE)) 'iae::grains)

(defmethod om::play-obj? ((self iae::IAE)) t)

(defmethod om::get-obj-dur ((self iae::IAE)) (iae::max-dur self))


;;;======================================================
;;; READ FROM IAE
;;;======================================================

(defun iae-info (iae)
  (iae-lib::iae_info_get_string (iaeengine-ptr iae) (oa::om-make-null-pointer)))


(om::defmethod! iae-descriptors ((self iae))
  :doc "Returns the list of all descriptor tracks computed in an IAE instance.

Note: some desciptor names used at initialization (e.g. MFCC, SpectralCrest, ...) produce more than one descriptor tracks (e.g. MFCC by default produces 12 tracks corresponding to 12 MFCC coefficients)."

  (descriptors self))

(om::defmethod! get-sound-descriptors ((self iae) src-index &optional (t1 0) (t2 nil))
  
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


;;;=========================
;;; DISPLAY
;;;=========================

;;; do that better with IDs etc.
(defmethod om::get-cache-display-for-draw ((self iae::IAE) box)
  (declare (ignore box))
  (append (call-next-method)
          (list (iae::iae-info self))))


(defmethod om::draw-mini-view ((self iae::IAE) (box t) x y w h &optional time)
  (call-next-method)
  (let ((display-cache (om::get-display-draw box)))
    (oa::om-with-font 
     (oa::om-def-font :font1 :size 8) 
     (loop for str in (om::string-lines-to-list (car display-cache))
           for y = 16 then (+ y 10) do 
           (om::om-draw-string 10 y str))
     )))


;;;============================================
;;; DATA-STREAM ACTIONS FOR IAE
;;;============================================


(defclass! IAE-grain (om::data-frame)
   ((om::date :accessor om::date :initarg :date :initform 0 :documentation "date/time of the grain")
    (source :accessor source :initarg :source :initform 0 :documentation "source num inside IAE")
    (pos :accessor pos :initarg :pos :initform 0 :documentation "position in source")
    (duration :accessor duration :initarg :duration :initform 100 :documentation "duration of the grain"))
   (:documentation "A granular-synthesis request for IAE, based on source/position data."))

(defclass! IAE-request (om::data-frame)
   ((om::date :accessor om::date :initarg :date :initform 0 :documentation "date/time of the grain")
    (descriptor :accessor descriptor :initarg :descriptor :initform 0 :documentation "the descriptor inside IAE/pipo")
    (value :accessor value :initarg :value :initform 0 :documentation "the value of the descriptor")
    (duration :accessor duration :initarg :duration :initform 100 :documentation "duration of the grain"))
   (:documentation "A granular-synthesis request for IAE, based on sound descritor value."))


;;; utils to generate random grains / requests
(defun make-IAE-grains (n &key (nsources 1) (maxpos 2500) (durtot 10000) (mindur 100) (maxdur 600))
  (sort 
   (loop for i from 1 to n collect
         (make-instance 'IAE-grain :date (random durtot)
                        :source (random (1- nsources))
                        :pos (random maxpos)
                        :duration (+ mindur (random (- maxdur mindur)))))
   '< :key 'om::date))

(defun gen-random-requests (n &key (descriptor 0) (minval 100) (maxval 1000) (durtot 10000) (mindur 50) (maxdur 300))
  (sort 
   (loop for i from 1 to n collect
         (make-instance 'IAE-request :date (random durtot)
                        :descriptor descriptor
                        :value (+ minval (random (- maxval minval)))
                        :duration (+ mindur (random (- maxdur mindur)))))
   '< :key 'om::date))


;;; OM wrappers:
(defmethod om::item-duration ((self iae::IAE-grain)) (iae::duration self))

(defmethod om::data-frame-text-description ((self iae::IAE-grain)) `("IAE GRAIN:" ,(format nil "~D in source ~A" (iae::pos self) (iae::source self))))
(defmethod om::data-frame-text-description ((self iae::IAE-request)) `("IAE REQUEST:" ,(format nil "desc. ~A = ~D" (iae::descriptor self) (iae::value self))))

(defmethod om::y-range-for-object ((self iae::IAE)) '(-1000 3000))

(defmethod om::get-frame-color ((self iae::IAE-grain)) (oa::om-make-color-alpha (om::get-midi-channel-color (1+ (iae::source self))) 0.5))
(defmethod om::get-frame-posy ((self iae::IAE-grain)) (+ 50 (iae::pos self)))
(defmethod om::get-frame-sizey ((self iae::IAE-grain)) 
  (or (getf (om::attributes self) :posy)
      (setf (getf (om::attributes self) :posy) (+ 200 (om::om-random -50 50)))))

(defmethod om::get-frame-color ((self iae::IAE-request)) (om::om-make-color-alpha (om::get-midi-channel-color (1+ (iae::descriptor self))) 0.5))
(defmethod om::get-frame-posy ((self iae::IAE-request)) (iae::value self))
(defmethod om::get-frame-sizey ((self iae::IAE-request)) 
  (or (getf (om::attributes self) :posy)
      (setf (getf (om::attributes self) :posy) (+ 200 (om::om-random -50 50)))))


;;; Returns a sound buffer with a grain from given pos in IAE
(defmethod! iae-synth ((self iae::IAE) source pos dur &key (gain 1.0) (attack 10) (release 10))
  :indoc '("An IAE instance" "source number" "position in source [ms]" "duration [ms]" "gain" "attack time [ms]" "release time [ms]")
  :doc "Synthesizes a grain (SOUND buffer) from IAE"
  :initvals '(nil 0 0 200 1.0 10 10)
  :outdoc '("sound")
  (when (iaeengine-ptr self)
    (let* ((*iae (iaeengine-ptr self))
           (nsamples (ceiling (* dur (iae::samplerate self) 0.001)))
           (omsnd (make-instance 'om::om-internal-sound :n-channels (channels self) :smpl-type :float
                                 :n-samples nsamples :sample-rate 44100))
           (**samples (om::make-audio-buffer (channels self) nsamples)))

;   Granular = 0,    asynchronous granular synthesis
;   Segmented = 1,   concatenative synthesis (needs at least 1 marker)
;   Synchronous = 2  synchronous granular synthesis (needs at least 2 markers)
      (iae-lib::iae_set_SynthMode *iae 0)

      (when (< source (length (sounds self)))
        (iae-lib::iae_set_sourceindex *iae source))

      (iae-lib::iae_set_Cyclic *iae nil)
      (iae-lib::iae_set_CenteredGrains *iae nil)
      (iae-lib::iae_set_Attack *iae (coerce attack 'double-float) 0.0d0)
      (iae-lib::iae_set_Release *iae (coerce release 'double-float) 0.0d0)
      (iae-lib::iae_set_position *iae (coerce pos 'double-float) 0.0d0)
      (iae-lib::iae_set_gain *iae (coerce gain 'double-float))
      ;(iae-lib::iae_set_positionvar *iae 1000.0d0)
      (iae-lib::iae_set_period *iae -0.0d0 0.0d0)
      (iae-lib::iae_set_duration *iae (coerce dur 'double-float) 0.0d0)
      (iae-lib::iae_trigger *iae)
      (iae-lib::iae_synth *iae nsamples **samples (channels self))
      (setf (om::buffer omsnd) (om::make-om-sound-buffer-gc :ptr **samples :count 1 :nch (channels self)))
      omsnd)))

;;; Returns a sound buffer with a grain from given set of descriptor values in IAE

;;; other option:
;;<request> can contain one or more triplets (desc,value,weight), where 'desc' is a descriptor number (as built-in teh IAE), and 'value' the targetted value for this descriptor. 'weight' is optional and will be set to 1.0 (maximum) by default.

(defmethod! iae-synth-desc ((self iae::IAE) descriptor value weight dur &key (gain 1.0) (attack 10) (release 10))
  
  :indoc '("An IAE instance" "descriptor number(s)" "requested value(s)" "weight(s)" "duration [ms]" "gain" "attack time [ms]" "release time [ms]")
  :initvals '(nil 0 0.0 1.0 200 1.0 10 10)
  :outdoc '("sound")
  :doc "Synthesizes a grain (SOUND buffer) from IAE, resquesting some value(s) for some given descriptor(s)."

  (when (iaeengine-ptr self)
    (let* ((*iae (iaeengine-ptr self))
           (nsamples (ceiling (* dur (iae::samplerate self) 0.001)))
           (omsnd (make-instance 'om::om-internal-sound :n-channels (channels self) :smpl-type :float
                                 :n-samples nsamples :sample-rate 44100))
           (**samples (om::make-audio-buffer (channels self) nsamples))
           ;; (framedescbuffer (fli::allocate-foreign-object :type :float :nelems (length (descriptors self))))
           )
      
;   Granular = 0,    asynchronous granular synthesis
;   Segmented = 1,   concatenative synthesis (needs at least 1 marker)
;   Synchronous = 2  synchronous granular synthesis (needs at least 2 markers)
      (iae-lib::iae_set_SynthMode *iae 1)

      (iae-lib::iae_set_Cyclic *iae nil)
      (iae-lib::iae_set_CenteredGrains *iae nil)
      (iae-lib::iae_set_Attack *iae (coerce attack 'double-float) 0.0d0)
      (iae-lib::iae_set_Release *iae (coerce release 'double-float) 0.0d0)
      (iae-lib::iae_set_period *iae -0.0d0 0.0d0)
      ;(iae-lib::iae_set_positionvar *iae 0.0d0)
      (iae-lib::iae_set_duration *iae (coerce dur 'double-float) 0.0d0)
      (iae-lib::iae_set_gain *iae (coerce gain 'double-float))
      (when (descriptors self)
        
        (let* ((n (length (descriptors self)))
               (vals (make-list n :initial-element 0.0))
               (weights (make-list n :initial-element 0.0)))
          
          (loop for desc in (om::list! descriptor)
                for i from 0 
                do 
                (if (>= desc n) (om-lisp:om-print-format "Error: no descriptor number ~D in IAE" (list desc) "OM-IAE")
                  (let ((value (float (or (if (listp value) (nth i value) value) 0.0)))
                        (weight (float (or (if (listp weight) (nth i weight) value) 1.0))))
                    (setf (nth descriptor vals) value)
                    (setf (nth descriptor weights) weight))
                  ))
          
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




;;;===============================
;;; AUDIO RENDERING
;;;===============================

(defun iae-add-grain (iae audiobuffer dur at)
  (when (iae::buffer-player iae)
    (let ((pos (round (* at (iae::samplerate iae)) 1000))
          (size (round (* dur (iae::samplerate iae)) 1000)))
      (dotimes (c (iae::channels iae))
        (dotimes (i size)
          (unless (>= i (om::bp-size (iae::buffer-player iae)))
            (setf (fli:dereference 
                   (fli:dereference (om::bp-buffer (iae::buffer-player iae)) :index c :type :pointer)
                   :index (+ pos i) :type :float)
                  (+ (fli:dereference 
                      (fli:dereference (om::bp-buffer (iae::buffer-player iae)) :index c :type :pointer) 
                      :index (+ pos i) :type :float)
                     (fli:dereference 
                      (fli:dereference (om::om-sound-buffer-ptr (om::buffer audiobuffer)) :index c :type :pointer)
                      :index i :type :float)))
            ))))))

(defmethod make-grain-from-frame ((self iae::IAE) (frame iae::IAE-grain))
  (iae::iae-synth self (source frame) (iae::pos frame) (iae::duration frame)))

(defmethod make-grain-from-frame ((self iae::IAE) (frame iae::IAE-request))
  (iae::iae-synth-desc self (iae::descriptor frame) (iae::value frame) (iae::duration frame)))


;;; This is the action performed when we "play" an IAE object
(defmethod om::get-computation-list-for-play ((object iae::IAE) &optional interval)
  (loop for frame in (remove-if #'(lambda (date) (or (< date (car interval)) (>= date (cadr interval))))
                                (om::data-stream-get-frames object) 
                                :key 'om::date)
        do (iae-add-grain object
                          (make-grain-from-frame object frame)
                          (duration frame) (om::date frame))
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
    (dotimes (c (om::bp-channels (iae::buffer-player self)))
      (dotimes (i (om::bp-size (iae::buffer-player self)))
        (setf (fli:dereference 
               (fli:dereference (om::bp-buffer (iae::buffer-player self)) :index c :type :pointer) 
               :index i :type :float)
              0.0)))))

(defmethod om::get-action-list-for-play ((object iae::IAE) interval &optional parent)
  (om::external-player-actions object interval parent))


(defmethod om::player-play-object ((self om::scheduler) (object iae::IAE) caller &key parent interval)
  (declare (ignore parent))
  (om::start-buffer-player (iae::buffer-player object) 
                       :start-frame (if (car interval)
                                        (round (* (car interval) (/ (iae::samplerate object) 1000.0)))
                                      (or (car interval) 0)))
  (call-next-method))

(defmethod om::player-stop-object ((self om::scheduler) (object iae::IAE))
  (let ((current-state (om::state self)))
    (om::stop-buffer-player (iae::buffer-player object))
    (unless (eq current-state :stop) 
      (iae-reset object))
    (call-next-method)))

(defmethod om::player-pause-object ((self om::scheduler) (object iae::IAE))
  (om::pause-buffer-player (iae::buffer-player object))
  (call-next-method))

(defmethod player-continue-object ((self om::scheduler) (object iae::IAE))
  (om::continue-buffer-player (iae::buffer-player object))
  (call-next-method))

(defmethod om::set-object-time ((self iae::IAE) time) 
  (iae-reset self)
  (om::jump-to-time (iae::buffer-player self) time)
  (call-next-method))




