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

(define-public python-cachecontrol
  (package
    (name "python-cachecontrol")
    (version "0.11.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "CacheControl" version))
       (sha256
        (base32
         "15bn8xll6z15h0zqhfjy1n8dn8p0fcb4m0rhnfanq63z7r2wpprp"))))
    (build-system python-build-system)
    (arguments
    `(#:configure-flags '("--single-version-externally-managed"
                          "--root=/"))) ;; avoid making eggs
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("python-lockfile" ,python-lockfile)))
    (home-page "https://github.com/ionrock/cachecontrol")
    (synopsis "The httplib2 caching algorithms packaged up for use with requests")
    (description "CacheControl is a port of the caching algorithms in httplib2 for
use with requests session object.")
    (license license:asl2.0)))

(define-public python-pytest-pep8
  (package
    (name "python-pytest-pep8")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-pep8" version))
       (sha256
        (base32
         "06032agzhw1i9d9qlhfblnl3dw5hcyxhagn7b120zhrszbjzfbh3"))))
    (build-system python-build-system)
    (arguments
     `(#:configure-flags '("--single-version-externally-managed"
                           "--root=/"))) ;; avoid making eggs
    (propagated-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-cache" ,python-pytest-cache)
       ("python-pep8" ,python-pep8)))
    (home-page "https://bitbucket.org/pytest-dev/pytest-pep8")
    (synopsis "Py.test plugin to check PEP8 requirements")
    (description "Pytest plugin for efficiently checking PEP8 compliance")
    (license license:expat)))

(define-public python-pytest-flakes
  (package
    (name "python-pytest-flakes")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-flakes" version))
       (sha256
        (base32
         "0flag3n33kbhyjrhzmq990rvg4yb8hhhl0i48q9hw0ll89jp28lw"))))
    (build-system python-build-system)
    (arguments
     `(#:configure-flags '("--single-version-externally-managed"
                           "--root=/") ;; avoid making eggs
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda _
             (zero? (system* "py.test")))))))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-pytest-cache" ,python-pytest-cache)
       ("python-pytest-pep8" ,python-pytest-pep8)))
    (propagated-inputs
     `(("python-pyflakes" ,python-pyflakes)))
    (home-page "https://github.com/fschulze/pytest-flakes")
    (synopsis "Py.test plugin to check source code with pyflakes")
    (description "Pytest plugin for efficiently checking python source with
pyflakes.")
    (license license:expat)))

(define-public python-natsort
  (package
    (name "python-natsort")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "natsort" version))
       (sha256
        (base32
         "1abld5p4a6n5zjnyw5mi2pv37gqalcybv2brjr2y6l9l2p8v9mja"))))
    (build-system python-build-system)
    (arguments
     `(#:configure-flags '("--single-version-externally-managed"
                           "--root=/"))) ;; avoid making eggs
    (native-inputs
     `(("python-hopythesis" ,python-hypothesis)
       ("python-pytest-cache" ,python-pytest-cache)
       ("python-pytest-flakes" ,python-pytest-flakes)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-pep8" ,python-pytest-pep8)))
    (home-page "https://github.com/SethMMorton/natsort")
    (synopsis "Sorts lists naturally")
    (description "Natural sorting for python")
    (license license:expat)))
