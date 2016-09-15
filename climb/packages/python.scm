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
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python))

(define-public python-ipymd
  (package
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
     `(#:tests? #f ;; FIXME some tests are failing
       #:configure-flags '("--single-version-externally-managed"
                           "--root=/"))) ;; avoid making eggs
    (native-inputs
     `(("python-pytest-cov" ,python-pytest-cov)
       ("python-odfpy" ,python-odfpy)
       ("python-ipython" ,python-ipython)))
    (propagated-inputs
     `(("python-pyaml" ,python-pyyaml)
       ;; XXX jsonschema should probably be propagated from ipython
       ("python-jsonschema" ,python-jsonschema)))
    (build-system python-build-system)
    (home-page "https://github.com/rossant/ipymd")
    (synopsis "Use the IPython notebook as an interactive Markdown editor")
    (description "Converts ipynb JSON objects to markdown")
    (license license:bsd-3)))

(define-public python-frontmatter
  (package
    (name "python-frontmatter")
    (version "0.3.1")
    (source
     (origin
       ;; No tagged releases and pip only has osx binary
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eyeseast/python-frontmatter")
             (commit "c6d769af")))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "0w5hipk489lh5b6hdwzbsvxzsvygaz6dlmvw510bkvdmzxlm0vcb"))))
    (arguments
     `(#:configure-flags '("--single-version-externally-managed"
                          "--root=/") ;; avoid making eggs
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "python" "test.py")))))))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-pyyaml" ,python-pyyaml)))
    (build-system python-build-system)
    (home-page "https://github.com/eyeseast/python-frontmatter")
    (synopsis "Parse and manage posts with YAML frontmatter")
    (description "Jekyll-style YAML front matter offers a useful way to add
arbitrary, structured metadata to text documents, regardless of type.")
    (license license:expat)))

(define-public python-jupyter
  ;; XXX meta-package, add dependencies
  (package
    (name "python-jupyter")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter" version))
       (sha256
        (base32
         "0pwf3pminkzyzgx5kcplvvbvwrrzd3baa7lmh96f647k30rlpp6r"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; no tests
    (home-page "https://jupyter.org/")
    (synopsis "Interactive data science")
    (description "The Jupyter Notebook is a web application that allows you to
create and share documents that contain live code, equations, visualizations and
explanatory text.  Uses include: data cleaning and transformation, numerical
simulation, statistical modeling, machine learning and much more.")
    (license license:bsd-3)))

(define-public python-ijson
  (package
    (name "python-ijson")
    (version "2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/isagalaev/ijson/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "15c39jf8skdzcn5zsiq7bsb4zhb8w1x2gbl5rzi6wy5dxg2jrdsx"))))
    (arguments
      `(#:configure-flags '("--single-version-externally-managed"
                            "--root=/") ;; avoid making eggs
        #:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda _
              (zero? (system* "python" "tests.py")))))))
    (build-system python-build-system)
    (home-page "https://github.com/isagalaev/ijson")
    (synopsis "Iterative JSON parser with Pythonic interface")
    (description "Ijson is an iterative JSON parser with a standard Python
iterator interface.")
    (license license:bsd-3)))

(define-public python-whichcraft
  (package
    (name "python-whichcraft")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "whichcraft" version))
              (sha256
               (base32
                "1aa9h0by4d51r64aad4pd2rn4wf510c9r179wwb8baspy7cx4mp7"))))
    (build-system python-build-system)
    (home-page "https://github.com/pydanny/whichcraft")
    (synopsis "Cross-platform cross-python shutil.which functionality")
    (description
     "Whichcraft is a shim of the shutil.which function that's designed to
work across multiple versions of Python")
    (license license:bsd-3)))

(define-public python-poyo
  (package
    (name "python-poyo")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "poyo" version))
              (sha256
               (base32
                "1f48ffl0j1f2lmgabajps7v8w90ppxbp5168gh8kh27bjd8xk5ca"))))
    (build-system python-build-system)
    (home-page "https://github.com/hackebrot/poyo")
    (synopsis "Lightweight YAML Parser for Python")
    (description
     "Poyo does not allow deserialization of arbitrary Python objects.
Supported types are str, bool, int, float, NoneType as well as dict and list
values.")
    (license license:expat)))

(define-public python-jinja2-time
  (package
    (name "python-jinja2-time")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jinja2-time" version))
              (sha256
               (base32
                "0h0dr7cfpjnjj8bgl2vk9063a53649pn37wnlkd8hxjy656slkni"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-arrow" ,python-arrow)
       ("python-jinja2" ,python-jinja2)))
    (home-page "https://github.com/hackebrot/jinja2-time")
    (synopsis "Jinja2 extension for dates and times")
    (description
     "Jinja2 extension that provides support for date and time objects.")
    (license license:expat)))

(define-public python-cookiecutter
  (package
    (name "python-cookiecutter")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cookiecutter" version))
              (sha256
               (base32
                "1vbbzid38qg9a7y7111qv6g50gq5msggxgss8gcwbkws1x454k8b"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-future" ,python-future)
       ("python-binaryornot" ,python-binaryornot)
       ("python-jinja2" ,python-jinja2)
       ("python-click" ,python-click)
       ("python-whichcraft" ,python-whichcraft)
       ("python-poyo" ,python-poyo)
       ("python-jinja2-time" ,python-jinja2-time)))
    (home-page "https://github.com/audreyr/cookiecutter")
    (synopsis "Command-line utility that creates projects from
cookiecutters (project templates)")
    (description
     "A command-line utility that creates projects from cookiecutters
(project templates), e.g. creating a Python package project from a Python
package project template.")
    (license license:bsd-3)))

(define-public python-sphinx-bootstrap-theme
  (package
    (name "python-sphinx-bootstrap-theme")
    (version "0.4.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinx-bootstrap-theme" version))
              (sha256
               (base32
                "0wmm292rpfzxaib7zf2j6kdl1dl2xzx303hx8sx8qsdy0pkmrk65"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; No tests.
       #:configure-flags '("--single-version-externally-managed" "--root=/")))
    (home-page "https://ryan-roemer.github.io/sphinx-bootstrap-theme/README.html")
    (synopsis "Bootstrap theme for Sphinx")
    (description "Sphinx theme that integrates the Bootstrap CSS / JavaScript
framework with various layout options, hierarchical menu navigation, and
mobile-friendly responsive design.")
    (license license:expat)))

(define-public python2-sphinx-bootstrap-theme
  (let ((base (package-with-python2 (strip-python2-variant
                                     python-sphinx-bootstrap-theme))))
    (package (inherit base)
             (native-inputs
              `(("python2-setuptools" ,python2-setuptools)
                ,@(package-native-inputs base))))))
