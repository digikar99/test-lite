# Why one more?

> Forget what I wrote below. [fiveam is excellent for what it is!](https://common-lisp-libraries.readthedocs.io/fiveam/#getting-started)

As of December 2019, [prove](https://github.com/fukamachi/prove) has been archived. One facility of prove that I fell in love were the `is-` forms: I haven't had the experience to use other facilities of interactive debugging (still getting a hang on condition-system), and suites (haven't worked on a big enough project),
and timing, and subtests, and fail and pass and finalize and more. May be it's just lack of experience. While prove has been succeeded by [rove](https://github.com/fukamachi/rove), rove doesn't seem to include the `is-` forms.

So, I made `test-lite` to 

- provide `is-` forms: `is is-condition is-error is-expand is-like is-print is-type is-values`; I did not see the need for `isnt` and `ok`.
- a very simple test system: `define-test run-test run-test-package remove-test`; these should be enough for projects under 500 lines of code

I do not know if one exists on [cliki](https://cliki.net/Test%20Framework) - most miss out on the `is-` forms. `prove` succeeds but I do not find it intuitive enough - may be it's just my lack of experience - and it has been archived.

