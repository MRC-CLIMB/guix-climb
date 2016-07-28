;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Marius Bakke <m.bakke@warwick.ac.uk>
;;;
;;; This file is NOT part of GNU Guix, but is supposed to be used with GNU
;;; Guix and thus has the same license.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (climb packages machine-learning)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages xorg))

(define-public dlib
  (package
    (name "dlib")
    (version "19.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://dlib.net/files/" name "-" version ".tar.bz2"))
       (sha256
        (base32
         "0a4xilzhb78ffpcyq2fipri80bmlgrl5ngr7r3jygdr9p7v3z4dn"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'build-test-suite
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir "../dlib-19.0")
             (mkdir-p "dlib/test/build")
             (chdir "dlib/test/build")
             (zero? (system* "cmake" ".."))
             (zero? (system* "cmake" "--build" "." "--config" "Release"))))
         (replace 'check
           (lambda _ (zero? (system* "./dtest" "--runall"))))
         (add-after 'check 'ascend-to-build-dir
           (lambda _ (chdir "../../../../build"))))))
    (inputs
     `(("openblas" ,openblas)
       ("libx11" ,libx11)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("zlib" ,zlib)))
    (synopsis "Toolkit for making machine learning and data analysis applications in C++")
    (description "Dlib is a modern C++ toolkit containing machine learning algorithms and
tools for creating complex software in C++ to solve real world problems.")
    (home-page "http://dlib.net")
    (license license:boost1.0)))
