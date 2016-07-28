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

(define-module (climb packages compression)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression))

(define-public gzstream
  (package
    (name "gzstream")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append ;; no versioned URL
             "http://www.cs.unc.edu/Research/compgeom/gzstream/gzstream.tgz"))
       (sha256
        (base32
         "00y19pqjsdj5zcrx4p9j56pl73vayfwnb7y2hvp423nx0cwv5b4r"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (mkdir-p lib)
               (mkdir-p include)
               (install-file "libgzstream.a" lib)
               (install-file "gzstream.h" include)
               #t))))))
    (home-page "http://www.cs.unc.edu/Research/compgeom/gzstream/")
    (synopsis "C++ library that provides the functionality of zlib in a C++ iostream")
    (description "gzstream is a small library for providing zlib functionality in a C++
iostream.  It is basically just a wrapper.")
    (license license:lgpl2.1)))
