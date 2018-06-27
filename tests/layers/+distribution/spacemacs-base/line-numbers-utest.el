(define-derived-mode linum-test-parent1 fundamental-mode "parent1")
(define-derived-mode linum-test-parent2 fundamental-mode "parent2")
(define-derived-mode linum-test-mode1 linum-test-parent1 "mode1")
(define-derived-mode linum-test-mode2 linum-test-parent2 "mode2")

(defmacro test--enable-linum-for-mode (mode)
  (declare (indent defun) (debug body))
  `(let ((major-mode ,mode))
     (spacemacs//linum-enabled-for-current-major-mode)))

(ert-deftest test-enable-linum-for-mode--1 ()
  (let ((dotspacemacs-line-numbers '(:size-limit-kb 1024)))
    (should (test--enable-linum-for-mode 'prog-mode))
    (should-not (test--enable-linum-for-mode 'linum-test-mode1))))

(ert-deftest test-enable-linum-for-mode--2 ()
  (let ((dotspacemacs-line-numbers '(:enabled-for-modes linum-test-parent1)))
    (should (test--enable-linum-for-mode 'linum-test-mode1))
    (should-not (test--enable-linum-for-mode 'linum-test-mode2))
    (should-not (test--enable-linum-for-mode 'prog-mode))))

(ert-deftest test-enable-linum-for-mode--3 ()
  (let ((dotspacemacs-line-numbers '(:disabled-for-modes linum-test-parent1)))
    (should-not (test--enable-linum-for-mode 'linum-test-mode1))
    (should-not (test--enable-linum-for-mode 'linum-test-mode2))
    (should (test--enable-linum-for-mode 'prog-mode))))

(ert-deftest test-enable-linum-for-mode--4 ()
  (let ((dotspacemacs-line-numbers '(:enabled-for-modes linum-test-parent1
                                     :disabled-for-modes linum-test-parent2)))
    (should (test--enable-linum-for-mode 'linum-test-mode1))
    (should-not (test--enable-linum-for-mode 'linum-test-mode2))
    (should-not (test--enable-linum-for-mode 'prog-mode))))

(ert-deftest test-enable-linum-for-mode--5 ()
  (let ((dotspacemacs-line-numbers '(:enabled-for-modes linum-test-parent1
                                     :disabled-for-modes linum-test-mode1)))
    (should-not (test--enable-linum-for-mode 'linum-test-mode1))))

(ert-deftest test-enable-linum-for-mode--6 ()
  (let ((dotspacemacs-line-numbers '(:enabled-for-modes linum-test-mode1
                                    :disabled-for-modes linum-test-parent1)))
    (should (test--enable-linum-for-mode 'linum-test-mode1))))

(ert-deftest test-enable-linum-for-mode--7 ()
  (let ((dotspacemacs-line-numbers '(:enabled-for-modes all)))
    (should (test--enable-linum-for-mode 'linum-test-mode1))))

(ert-deftest test-enable-linum-for-mode--8 ()
  (let ((dotspacemacs-line-numbers '(:enabled-for-modes all
                                     :disabled-for-modes linum-test-parent1)))
    (should-not (test--enable-linum-for-mode 'linum-test-mode1))
    (should (test--enable-linum-for-mode 'linum-test-mode2))))

(ert-deftest test-enable-linum-for-mode--9 ()
  (let ((dotspacemacs-line-numbers '(:disabled-for-modes text-mode)))
    (should-not (test--enable-linum-for-mode 'text-mode))
    (should (test--enable-linum-for-mode 'prog-mode))))
