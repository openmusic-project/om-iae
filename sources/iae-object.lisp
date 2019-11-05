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

(defclass! IAE (om::om-cleanup-mixin)   ; om::data-stream
 ((iaeengine-ptr :accessor iaeengine-ptr :initform nil)
  (sounds :initarg :sounds :accessor sounds :initform nil :documentation "a sound or list of sounds to build the IAE container on")
  (channels :accessor channels :initform 1 :documentation "number of channels for audio output")
  (samplerate :accessor samplerate :initform 44100 :documentation "sample rate for audio output")
  (pipo-module :accessor pipo-module :initform "descr" :documentation "name of a pipo module for sound analysis") 
  (descriptors :accessor descriptors :initform nil)
  (desc-tracks :accessor desc-tracks :initform nil)
  (segmentation :accessor segmentation :initform nil :documentation "segmentation params"))
 (:documentation "IAE is a multi-track container for sounds and sound descriptions are stored data. 

 - Tracks can be computed and segmented using 'pipo' modules: \"desc\" \"ircamdescriptor\" \"slice:fft\" \"mfcc\" \"<desc,mfcc>\" ... 

 - Segmentation is computed from the <segmentation> parameter which can be a chop-size or a list (module (param1 val1 (param2 val2) ...) where 'module' is one of \"chop\", \"onseg\", or \"gate\".
If <segmentation> is an integer value (chop-size), this value is considered the size (in milliseconds) for the \"chop\" segmentation mode.
"

  ) 
)


(defmethod om::om-cleanup ((self iae::IAE))
  (when (iae::iaeengine-ptr self)
    (om::om-print-dbg "deleting engine of ~A [~A]" (list self (iae::iaeengine-ptr self)) "GC")
    (iae-lib::iae_delete (iae::iaeengine-ptr self))
    (setf (iae::iaeengine-ptr self) nil)
    ))

;;; called each time an instance is created
;;; => mostly memory allocations
(defmethod initialize-instance :after ((self iae::IAE) &rest initargs)
  (om::om-print-dbg "Initializing IAE for ~A" (list self) "IAE")
  (setf (iae::iaeengine-ptr self) 
        (iae-lib::iae_new  (iae::samplerate self) 512 (iae::channels self) 1))
  )
    
;;======================================================
;; INITIALIZATION OF THE PIPO MODULE
;; module-name can be: "desc" "ircamdescriptor" "slice:fft" "mfcc" "<desc,mfcc>" "...:chop"
;;======================================================

(defmethod iae-init-pipo ((self iae))

  (let* ((*iae (iaeengine-ptr self))
         (main-pipo (if (listp (iae::pipo-module self)) "ircamdescriptor" (iae::pipo-module self)))
         (seg-list (if (numberp (segmentation self)) 
                       `("chop" ("size" ,(segmentation self)))
                     (segmentation self)))
         (pipo-string (if seg-list (concatenate 'string main-pipo ":" (car seg-list)) main-pipo)))

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
          
          (loop for param in (cdr seg-list) do
                ;;; set, e.g. "chop.size" etc.
                (let ((param-name (concatenate 'string (car seg-list) "." (car param))))
                  (iae-lib::iae_pipo_param_set_int *iae param-name 0 (cadr param))))
          
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
  '(iae::channels iae::pipo-module iae::segmentation))

;;;======================================================
;;; READ FROM IAE
;;;======================================================

(defun iae-info (iae)
  (iae-lib::iae_info_get_string (iaeengine-ptr iae) (oa::om-make-null-pointer)))


;;;=========================
;;; DESCRIPTORS
;;;=========================


(om::defmethod! iae-descriptors ((self iae))
  :doc "Returns the names of all descriptor tracks computed in an IAE instance.

 Note: some desciptor names used at initialization (e.g. MFCC, SpectralCrest, ...) produce more than one descriptor tracks (e.g. MFCC by default produces 12 tracks corresponding to 12 MFCC coefficients)."

  (descriptors self))


(om::defmethod! get-sound-descriptors ((self iae) src-index &optional (t1 0) (t2 nil))
  
  :indoc '("An IAE instance" "source index" "min time" "max time")
  :initvals '(nil 0 0 nil)
  :outdoc '("a list of time+descriptor value lists")
  :doc "Returns the descriptor values for segment <seg-index> in <self>."
  
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


(defmethod! get-segment-descriptors ((self iae::IAE) (src-index integer) (seg-index integer))
  
  :indoc '("An IAE instance" "source index" "segment index in source")
  :initvals '(nil 0 0)
  :outdoc '("a list (time of the segment / list of descriptor values)")
  :doc "Returns the time and descriptor values for segment <seg-index> in <src-index>."
  
    (when (and (iaeengine-ptr self)
               (descriptors self))
    
      (let* ((*iae (iaeengine-ptr self))
             (n (length (descriptors self)))
             (framedescbuffer (cffi::foreign-alloc :float :count n)))

        (unwind-protect 
            (let* ((time (iae-lib::iae_get_descriptor_data *iae src-index seg-index framedescbuffer))
                   (data (loop for i from 0 to (1- n) collect
                               (cffi::mem-aref framedescbuffer :float i))))
              (list time data))

          (cffi::foreign-free framedescbuffer))
      
        )))


;;;=========================
;;; KNN
;;;=========================

(defmethod! iae-knn ((self iae::IAE) descriptor value weight k &oprional radius)
  
  :indoc '("An IAE instance" "descriptor number(s)" "requested value(s)" "weight(s)" "number of soultions" "max radius of the search domain")
  :initvals '(nil 0 0.0 1.0 3 nil)
  :outdoc '("a list of candidate (source-index segment-index)")
  :doc "Searchs for k-best candidates (source-index and segment-position) in a IAE buffer, matching some value(s) for some given weighted descriptor(s)."

  (when (and (iaeengine-ptr self)
             (descriptors self))
    
    (let* ((*iae (iaeengine-ptr self))
           (n (length (descriptors self)))
           (vals (make-list n :initial-element 0.0))
           (weights (make-list n :initial-element 0.0)))
          
      (loop for desc in (om::list! descriptor)
            for i from 0 
            do 
            (if (>= desc n) (om-lisp:om-print-format "Error: no descriptor number ~D in IAE" (list desc) "OM-IAE")
              (let ((value (float (or (if (consp value) (nth i value) value) 0.0)))
                    (weight (float (or (if (consp weight) (nth i weight) weight) 1.0))))
                (setf (nth desc vals) value)
                (setf (nth desc weights) weight))
              ))

      (let ((value-array (cffi::foreign-alloc :float :initial-contents vals))
            (weight-array (cffi::foreign-alloc :float :initial-contents weights)))
        
        (unwind-protect 
                
            (progn 
              
              (iae-lib::iae_set_target *iae n value-array)
              (iae-lib::iae_set_weight *iae n weight-array)
              (iae-lib::iae_set_k *iae k)

              (when radius ;; typically r = [0 - 4]
                ;;; !! may lead to a list smaller than k
                (iae-lib::iae_set_radius *iae radius))  
              
              (iae-lib::iae_select_new *iae nil)  
              
              (loop for i from 0 to (1- k) collect
                    (let* ((selsrc (iae-lib::iae_get_SelectedSourceIndex *iae i))
                           (selind (iae-lib::iae_get_SelectedSegmentIndex *iae i)))
                      (list selsrc selind)))
              )
          
          (cffi::foreign-free value-array)
          (cffi::foreign-free weight-array)
          
          ))
      )))
              

;;;=========================
;;; SYNTH
;;;=========================

;;; Returns a sound buffer with a grain from given pos in IAE
(defmethod! iae-synth ((self iae::IAE) source marker offset dur &key (gain 1.0) (attack 10) (release 10) (other-iae-params))
  :indoc '("An IAE instance" "source number" 
           "position in source [marker-id]" 
           "time-position in source [ms]" 
           "duration [ms]" 
           "gain" "attack time [ms]" "release time [ms]")
  :doc "Synthesizes a grain (SOUND buffer) from IAE.

- <source> is the source number in IAE (must be inferior to the total number of sources)
- <marker> is the marker/segment index in <source> (must be inferior to the total number of segments)
- <offset> is an offset in milliseconds relative to <marker>, or to the beginning of <source> if <marker> = NIL.
- <dur> is the duration of the output grain.

- <other-params> is a lits of list of the form ((\"param\" value) ...) corresponding to the parameters of IAE/MuBu.
"
  :initvals '(nil 0 nil 0 200 1.0 10 10)
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
      (iae-lib::iae_set_SynthMode *iae 1)

      (when (< source (length (sounds self)))
        (iae-lib::iae_set_sourceindex *iae source))
      
      ;;; general params
      (iae-lib::iae_set_Cyclic *iae nil)
      (iae-lib::iae_set_CenteredGrains *iae nil)
      (iae-lib::iae_set_Attack *iae (coerce attack 'double-float) 0.0d0)
      (iae-lib::iae_set_Release *iae (coerce release 'double-float) 0.0d0)
      (iae-lib::iae_set_period *iae -0.0d0 0.0d0)
      (iae-lib::iae_set_duration *iae (coerce dur 'double-float) 0.0d0)
      (iae-lib::iae_set_gain *iae (coerce gain 'double-float))
      
      ;;; mode-specific
      (if marker
          
          (progn (iae-lib::iae_set_markerindex *iae marker)
            (iae-lib::iae_set_offset *iae (coerce offset 'double-float)))
        
        (iae-lib::iae_set_position *iae (coerce offset 'double-float) 0.0d0))

      ; (iae-lib::iae_set_positionvar *iae 1000.0d0)
      
      
      ;;; generates the grain
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

      ;;; general params
      (iae-lib::iae_set_Cyclic *iae nil)
      (iae-lib::iae_set_CenteredGrains *iae nil)
      (iae-lib::iae_set_Attack *iae (coerce attack 'double-float) 0.0d0)
      (iae-lib::iae_set_Release *iae (coerce release 'double-float) 0.0d0)
      (iae-lib::iae_set_period *iae -0.0d0 0.0d0)
      (iae-lib::iae_set_duration *iae (coerce dur 'double-float) 0.0d0)
      (iae-lib::iae_set_gain *iae (coerce gain 'double-float))
      
      ;;; mode-specific
      (when (descriptors self)
        
        (let* ((n (length (descriptors self)))
               (vals (make-list n :initial-element 0.0))
               (weights (make-list n :initial-element 0.0)))
          
          (loop for desc in (om::list! descriptor)
                for i from 0 
                do 
                (if (>= desc n) (om-lisp:om-print-format "Error: no descriptor number ~D in IAE" (list desc) "OM-IAE")
                  (let ((value (float (or (if (consp value) (nth i value) value) 0.0)))
                        (weight (float (or (if (consp weight) (nth i weight) weight) 1.0))))
                    (setf (nth desc vals) value)
                    (setf (nth desc weights) weight))
                  ))
          
          (let ((value-array (cffi::foreign-alloc :float :initial-contents vals))
                (weight-array (cffi::foreign-alloc :float :initial-contents weights)))
        
            (unwind-protect 
                (progn 
                  (iae-lib::iae_set_target *iae n value-array)
                  (iae-lib::iae_set_weight *iae n weight-array)
                  (iae-lib::iae_set_k *iae 1)
                  ;;; generates the grain
                  (iae-lib::iae_select_new *iae T)   ;; T = force-trigger
                  (iae-lib::iae_synth *iae nsamples **samples (channels self)))
              
              (cffi::foreign-free value-array)
              (cffi::foreign-free weight-array))
            )))
      
      (setf (om::buffer omsnd) (om::make-om-sound-buffer-gc :ptr **samples :count 1 :nch (channels self)))
      
      omsnd)))


;;;=========================
;;; DISPLAY
;;;=========================

;;; do that better with IDs etc.
(defmethod om::get-cache-display-for-draw ((self iae::IAE) box)
  (declare (ignore box))
  (append (call-next-method)
          (list (iae::iae-info self))))


(defmethod om::draw-mini-view ((self iae::IAE) (box t) x y w h &optional time)
  (let ((display-cache (om::get-display-draw box)))
    (oa::om-with-font 
     (oa::om-def-font :font1 :size 8) 
     (loop for str in (om::string-lines-to-list (car display-cache))
           for y = 16 then (+ y 10) do 
           (om::om-draw-string 10 y str))
     )))


