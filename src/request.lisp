(in-package :weaver)

(defclass request ()
  ((headers-in :initarg :headers-in
               :reader headers-in)
   (method :initarg :method
           :reader request-method)
   (uri :initarg :uri
        :reader request-uri))
  )
