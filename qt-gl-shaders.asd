;;;; qt-gl-shaders.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:qt-gl-shaders
  :description "Describe qt-gl-shaders here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:qt
               #:qtools
               #:qtgui
               #:qtcore
               #:qtopengl
               #:cl-opengl
               #:cl-glu
               #:trivial-garbage
               #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "qt-gl-shaders")))

