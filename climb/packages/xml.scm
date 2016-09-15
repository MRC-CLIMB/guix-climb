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

(define-module (climb packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python))

(define-public python2-xmlbuilder
  (package
    (name "python2-xmlbuilder")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "xmlbuilder" version))
              (sha256
               (base32
                "0p52f7iy8ym3a720lc282zmpjdy8jvmzjr7zprxaijp9pwqk5syd"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (zero? (system* "nosetests" "-v")))))))
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)
       ("python2-nose" ,python2-nose)))
    (home-page "https://pypi.python.org/pypi/xmlbuilder")
    (synopsis "Pythonic way to create xml/(x)html files")
    (description
     "XMLBuilder is a smalll library built on top of
@code{ElementTree.TreeBuilder} to make XML files creation more pythonic.
XMLBuilder uses @code{with} statement and attribute accesses to define
XML document structure.  This package works with Python 2.x only.")
    (license license:lgpl3)))
