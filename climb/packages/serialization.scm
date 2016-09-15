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

(define-module (climb packages serialization)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python))

(define-public python-avro
  (package
    (name "python-avro")
    (version "1.8.1") ; This must be updated for python2 package too.
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "avro-python3" version))
              (sha256
               (base32
                "0paksy5a5hw7whrragmnp21dibw5gyc9ancdr4nw2kwp1gb5yjy1"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Tests missing or incomplete in released versions; git only.
    (propagated-inputs
     `(("python-simplejson" ,python-simplejson)))
    (home-page "https://avro.apache.org/")
    (synopsis "Python implementation of the Avro data serialization system")
    (description
     "Avro is a data serialization system and RPC framework with rich data
structures.  This package provides the python version.")
    (license license:asl2.0)))

(define-public python2-avro
  (let ((base (package-with-python2 (strip-python2-variant python-avro))))
    (package (inherit base)
             (version "1.8.1")
             (source
              (origin
                (method url-fetch)
                (uri (pypi-uri "avro" version))
                (sha256
                 (base32
                  "13iyhclhm5fs6z0mbg7n6pabfsvn22sravm1dr54hwzg8j1g89is")))))))
