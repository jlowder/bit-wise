(defsystem bit-wise
    :name "bit-wise"
    :version "0.1.0"
    :author "Jason Lowdermilk <jlowdermilk@gmail.com>"
    :licence "MIT"
    :description "A collection of bit-level file utilities"
    :depends-on (:rmatch)
    :components
    ((:module "src"
              :components 
              ((:file "bit-wise")))))
