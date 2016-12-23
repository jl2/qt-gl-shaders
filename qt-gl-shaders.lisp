;;;; qt-gl-shaders.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:qt-gl-shaders)

(named-readtables:in-readtable :qtools)
(declaim (optimize (speed 3) (safety 2) (size 0) (debug 3)))

(defparameter *fps* 60)
(define-widget shader-example (QGLWidget)
  ((vbuff :accessor vertex-buffer)
   (ibuff :accessor index-buffer)
   (vs :accessor vertex-shader)
   (fs :accessor fragment-shader)
   (va :accessor vertex-array)
   (program :accessor program)
   (angle :accessor angle :initform 0.0))

  ;; (vertex-buffer :accessor vertex-buffer)
  ;;  (index-buffer :accessor index-buffer)
  ;;  (vertex-shader)
  ;;  (fragment-shader)
  ;;  (vertex-array)
  ;;  (program)
  ;;  (angle :initform 0.0))
  (:documentation "The scene-drawer widget draws a cube using the parameters specified in scene."))

(defvar *shader-vao-vertex-program*
  "#version 330

// The location is 0 because that's the vertex attribute we associate with vertex positions.
layout (location = 0) in vec3 in_Position;

uniform mat4 projectionMatrix;
uniform float angle;

// This is interpolated and used in the fragment shader.
smooth out vec2 pos;

void main()
{
  mat2 rotationMatrix = mat2(cos(angle), sin(angle), -sin(angle), cos(angle));
  float scaleFactor = 1.0 + 0.5 * sin(1.75 * angle);
  vec2 vertPos = scaleFactor * rotationMatrix * in_Position.xy;
  pos = vertPos * 5.0;

  gl_Position = projectionMatrix * vec4(vertPos, 0.0, 1.0); 
} 
")

(defvar *shader-vao-fragment-program*
  "#version 330

out vec4 out_Color;
smooth in vec2 pos;

uniform float angle;

void main() 
{
  mat2 rotationMatrix = mat2( cos(angle), sin(angle), -sin(angle), cos(angle) );
  vec2 rpos = mod(rotationMatrix * pos, 2.0 );
  
  if ((rpos.x > 1.0 && rpos.y > 1.0 ) || (rpos.x < 1.0 && rpos.y < 1.0))
    out_Color = vec4(0.1, 0.1, 0.1, 1.0); 
  else
    out_Color = vec4(0.5, 0.5, 0.7, 1.0);
} 
")


(define-subwidget (shader-example timer) (q+:make-qtimer shader-example)
  (setf (q+:single-shot timer) nil))

(define-initializer (shader-example setup)
  (q+:start timer (round (/ 1000 *fps*)))
  (setf (q+:auto-fill-background shader-example) nil)
  (setf (q+:auto-buffer-swap shader-example) nil))

(define-slot (shader-example tick) ()
  (declare (connected timer (timeout)))
  (when (slot-boundp shader-example 'program)
    (let ((seconds-per-revolution 6)) 
             (incf  (angle  shader-example)
                    (/ (* 2 pi) (* 60 seconds-per-revolution))))
    (gl:uniformf (gl:get-uniform-location (program  shader-example) "angle") (angle  shader-example))
    (q+:repaint shader-example)))

(define-override (shader-example initialize-G-L) ()
  (flet ((get* (x &optional (j 0 jp))
           (multiple-value-bind (r e) (ignore-errors (if jp
                                                         (gl:get* x j)
                                                         (gl:get* x)))
             (or e r))))
    (format t "GL version: ~a~%" (get* :version))
    (format t "GLSL version: ~a~%" (get* :shading-language-version))
    (format t "GL vendor: ~a~%" (get* :vendor))
    (format t "GL renderer: ~a~%" (get* :renderer))
    (format t "GL extensions: ~a~%" (get* :extensions))
    )
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test)

  (let ((buffers (gl:gen-buffers 2)))
    (setf (vertex-buffer  shader-example) (elt buffers 0)
          (index-buffer  shader-example) (elt buffers 1)))
  (gl:bind-buffer :array-buffer (vertex-buffer  shader-example))
  (let ((arr (gl:alloc-gl-array :float 12))
        (verts #(-0.5f0 -0.5f0 0.0f0 
                 -0.5f0 0.5f0 0.0f0 
                 0.5f0 -0.5f0 0.0f0 
                 0.5f0 0.5f0 0.0f0)))
    (dotimes (i (length verts))
      (setf (gl:glaref arr i) (aref verts i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  ;; 0 is always reserved as an unbound object.
  (gl:bind-buffer :array-buffer 0)

  ;; An element array buffer stores vertex indices. We fill it in the
  ;; same way as an array buffer.
  (gl:bind-buffer :element-array-buffer (index-buffer  shader-example))
  (let ((arr (gl:alloc-gl-array :unsigned-short 6))
        (indexes #(0 2 1 1 2 3)))
    (dotimes (i (length indexes))
      (setf (gl:glaref arr i) (aref indexes i)))
    (gl:buffer-data :element-array-buffer :static-draw arr)
    (gl:free-gl-array arr))
  (gl:bind-buffer :element-array-buffer 0)

  ;; Vertex array objects manage which vertex attributes are
  ;; associated with which data buffers. 
  (setf (vertex-array  shader-example) (gl:gen-vertex-array))
  (gl:bind-vertex-array (vertex-array  shader-example))

  ;; To associate our VBO data with this VAO, we bind it, specify
  ;; which vertex attribute we want to associate it with, and specify
  ;; where the data comes from.
  (gl:bind-buffer :array-buffer (vertex-buffer  shader-example))
  ;; In this program, we use attribute 0 for position. If you had
  ;; per-vertex normals, you could use a different attribute for those
  ;; as well.
  (gl:enable-vertex-attrib-array 0)
  ;; Using a null pointer as the data source indicates that we want
  ;; the vertex data to come from the currently bound array-buffer.
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))

  ;; To associate an element array with this VAO, all we need to do is
  ;; bind the element array buffer we want to use.
  (gl:bind-buffer :element-array-buffer (index-buffer  shader-example))

  ;; Once we're done, we can unbind the VAO, and rebind it when we want to render it.
  (gl:bind-vertex-array 0)

  ;; A program object is a collection of shader objects to be used
  ;; together in a single pipeline for rendering objects. To create a
  ;; program, you first create the individual shaders. Then you attach
  ;; the shaders to the program and link the program together.
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader)))
    (setf (vertex-shader  shader-example) vs)
    (setf (fragment-shader  shader-example) fs)
    (gl:shader-source vs *shader-vao-vertex-program*)
    (gl:compile-shader vs)
    (gl:shader-source fs *shader-vao-fragment-program*)
    (gl:compile-shader fs)
    ;; If the shader doesn't compile, you can print errors with:
    ;; (print (gl:get-shader-info-log vs))
    ;; (print (gl:get-shader-info-log fs))

    (setf (program  shader-example) (gl:create-program))
    ;; You can attach the same shader to multiple different programs.
    (gl:attach-shader (program  shader-example) vs)
    (gl:attach-shader (program  shader-example) fs)
    ;; Don't forget to link the program after attaching the
    ;; shaders. This step actually puts the attached shader together
    ;; to form the program.
    (gl:link-program (program  shader-example))
    ;; If we want to render using this program object, or add
    ;; uniforms, we need to use the program. This is similar to
    ;; binding a buffer.
    (gl:use-program (program  shader-example))))

(define-override (shader-example resize-g-l) (width height)
  )

(define-override (shader-example paint-g-l paint) ()
  "Handle paint events."
  
  (let* ((max-radius zoom)
         (width (q+:width shader-example))
         (height (q+:height shader-example))
         (x-aspect-ratio (if (< height width)
                             (/ height width 1.0d0)
                             1.0d0))
         (y-aspect-ratio (if (< height width)
                             1.0d0
                             (/ width height 1.0d0))))

    (with-finalizing 
        ;; Create a painter object to draw on
        ((painter (q+:make-qpainter shader-example)))

      (q+:begin-native-painting painter)

      (gl:viewport 0 0 width height)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      ;; Ensure that projection matrix ratio always matches the window size ratio,
      ;; so the polygon will always look square.
      (let ((right (max (float (/ width height)) 1.0))
            (top (max (float (/ height width)) 1.0)))
        (glu:ortho-2d (- right) right (- top) top))
      (when (program  shader-example)
        (let ((proj-mat (gl:get-float :projection-matrix)))
          (gl:uniform-matrix 
           (gl:get-uniform-location (program  shader-example) "projectionMatrix") 
           4 
           (vector proj-mat))))
      (gl:matrix-mode :modelview)
      (gl:load-identity)

      (gl:clear-color 0.0 0.0 0.2 1.0)
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      
      ;; Since we never use any other program object, this is unnecessary
      ;; in this program. Typically, though, you'll have multiple program
      ;; objects, so you'll need to 'use' each one to activate it.
      (gl:use-program (program  shader-example))
      (gl:bind-vertex-array (vertex-array  shader-example))
      
      ;; This call actually does the rendering. The vertex data comes from
      ;; the currently-bound VAO. If the input array is null, the indices
      ;; will be taken from the element array buffer bound in the current
      ;; VAO.
      (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)

      (q+:swap-buffers shader-example)
      (q+:end-native-painting painter)
      ;; (trivial-garbage:gc)
      )))

(define-override (shader-example key-press-event key-press) (ev)
  (format t "Key press event: ~a ~a~%" ev (q+:key ev))
  )

(define-override (shader-example mouse-press-event mouse-press) (ev)
  (format t "Mouse press event: ~a ~a ~a~%" (q+:x ev) (q+:y ev) (q+:button ev)))

(define-override (shader-example mouse-release-event mouse-release) (ev)
  (format t "Mouse press event: ~a ~a ~a~%" (q+:x ev) (q+:y ev) (q+:button ev)))

(define-override (shader-example mouse-move-event mouse-move) (ev)
  (format t "Mouse press event: ~a ~a~%" (q+:x ev) (q+:y ev)))

(define-widget main-window (QMainWindow)
  ())

(define-override (main-window close-event) (ev)
  (when (slot-boundp shader-example 'vs)
   (gl:delete-shader (vertex-shader  shader-example)))
  (when (slot-boundp shader-example 'fs)
    (gl:delete-shader (fragment-shader  shader-example)))
  (when (slot-boundp shader-example 'program)
   (gl:delete-program (program  shader-example)))

  (when (slot-boundp shader-example 'vbuff)
    (gl:delete-buffers (list (vertex-buffer shader-example) (index-buffer shader-example))))
  (when (slot-boundp shader-example 'va)
   (gl:delete-vertex-arrays (list (vertex-array shader-example))))
  (q+:accept ev))


(define-menu (main-window File)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Interactively view and manipulate FFT data.")))

(define-subwidget (main-window scene-controller) (make-instance 'shader-example)
  "The central controller-widget.")


(define-initializer (main-window setup)
  "Set the window title and set the fft-controls to be the central widget."
  (setf (q+:window-title main-window) "Interactive FFT Explorer")
  (setf (q+:central-widget main-window) scene-controller)
  (q+:set-focus scene-controller))

(defun main ()
  "Create the main window."
  (with-main-window (window (make-instance 'main-window))))
