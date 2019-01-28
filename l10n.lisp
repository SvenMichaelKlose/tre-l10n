(var *l10ns* (make-hash-table :test #'eq))
(var *l10n-package* nil)

(@ (i *available-languages*)
  (= (href *l10ns* i) (make-hash-table :test #'eq)))

(defmacro def-l10n (lang id args &body body)
  (print-definition `(def-l10n ,lang ,id))
  (| *l10n-package*
     (error "*L10N-PACKAGE* is unset."))
  (| (href *compile-time-l10ns* lang)
     (error "Language ~A is not defined." lang))
  (alet (packaged-l10n-id id)
    (& (href (href *compile-time-l10ns* lang) !)
       (error "Localisation ~A ~A is already defined." lang !))
    (= (href (href *compile-time-l10ns* lang) !) (list args))
    (& (not (eq lang 'en))
       (not (href (href *compile-time-l10ns* 'en) !))
       (error "Localisation ~A ~A is not in EN." lang !))
    `{(& (href (href *l10ns* ',lang) ',!)
         (error "Localisation ~A ~A is already defined." ',lang ',!))
      (= (href (href *l10ns* ',lang) ',!) #'(,args ,@body))}))

(fn get-localiser (id)
  (href (href *l10ns* *language*) id))

(fn call-localiser (id &rest args)
  (fast-typography (apply (get-localiser id) args)))

(defmacro l10n (id &rest args)
  (print-definition `(l10n ,id))
  (| *l10n-package*
     (error "*L10N-PACKAGE* is unset."))
  (| (href (href *compile-time-l10ns* 'en) (packaged-l10n-id id))
     (error "Localisation ~A is not defined." id))
  (alet (packaged-l10n-id id)
    (= (href *used-l10ns* !) t)
    `(call-localiser ',! ,@args)))
