// This file is intended to be included into an array of 'BuiltinIProc'

{ "read-eval-print-loop",
    "()",
    "(let loop ()"
    "  (display \">> \")"
    "  (display (eval (read) (interaction-environment)))"
    "  (newline)"
    "  (loop))",
    },

{ "rsc-macro-transformer",
    "(fun)",
    "(sc-macro-transformer"
    "  (lambda (body usage-env)"
    "   (capture-syntactic-environment"
    "    (lambda (trans-env)"
    "     (make-syntactic-closure usage-env ()"
    "      (fun body trans-env))))))",
    },

{ "close-syntax",
    "(form env)",
    "(make-syntactic-closure env () form)",
    },
