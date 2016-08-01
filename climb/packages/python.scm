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

(define-module (climb packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python))

(define-public python-reportlab
  ;; XXX non-deterministic build
  (package
    (name "python-reportlab")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "reportlab" version))
              (sha256
               (base32
                "0rz2pg04wnzjjm2f5a8ik9v8s54mv4xrjhv5liqjijqv6awh12gl"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pillow" ,python-pillow)))
    (home-page "http://www.reportlab.com")
    (synopsis "Python library for generating PDFs and graphics")
    (description
     "This is the ReportLab PDF Toolkit.  It allows rapid creation of rich PDF documents,
and also creation of charts in a variety of bitmap and vector formats.")
    (license license:bsd-3)))

(define-public python-odfpy
  (package
    (name "python-odfpy")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "odfpy" version))
       (sha256
        (base32
         "1a6ms0w9zfhhkqhvrnynwwbxrivw6hgjc0s5k7j06npc7rq0blxw"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (system* "make" "-C" "tests"))))))
    (build-system python-build-system)
    (home-page "https://github.com/eea/odfpy")
    (synopsis "API for OpenDocument in Python")
    (description "Collection of utility programs written in Python to manipulate
OpenDocument 1.2 files.")
    (license (list license:asl2.0 license:gpl2))))

(define-public python-ipymd
  (package
    ;; XXX non-deterministic build
    (name "python-ipymd")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ipymd" version))
       (sha256
        (base32
         "092x1k478bgxaa9b7kzxy1a4vvsdcj1fvx4cdyvf94az6pqncf8b"))))
    (arguments
     `(#:tests? #f)) ;; FIXME some tests are failing
    (native-inputs
     `(("python-pytest-cov" ,python-pytest-cov)
       ("python-jsonschema" ,python-jsonschema)
       ("python-odfpy" ,python-odfpy)
       ("python-ipython" ,python-ipython)))
    (propagated-inputs
     `(("python-pyaml" ,python-pyyaml)))
    (build-system python-build-system)
    (home-page "https://github.com/rossant/ipymd")
    (synopsis "Use the IPython notebook as an interactive Markdown editor")
    (description "Converts ipynb JSON objects to markdown")
    (license license:bsd-3)))
