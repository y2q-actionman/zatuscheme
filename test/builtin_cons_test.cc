#include "zs.hh"
#include "test_util.hh"

int main(){
  zs_init();

  check_e("(pair? '(a . b))", "#t");
  check_e("(pair? '(a b c))", "#t");
  check_e("(pair? '())", "#f");
  check_e("(pair? #(a b))", "#f");

  check_e("(cons 'a '())", "(a)");
  check_e("(cons '(a) '(b c d))", "((a) b c d)");
  check_e("(cons \"a\" '(b c))", "(\"a\" b c)");
  check_e("(cons 'a 3)", "(a . 3)");
  check_e("(cons '(a  b) 'c)", "((a b) . c)");
  
  check_e("(car '(a b c))", "a");
  check_e("(car '((a) b c d))", "(a)");
  check_e("(car '(1 . 2))", "1");
  check_e_undef("(car '())");
  
  check_e("(cdr '((a) b c d))", "(b c d)");
  check_e("(cdr '(1 . 2))", "2");
  check_e_undef("(cdr '())");

  eval_text("(define tmp (cons 'a 'b))");
  check_e("tmp", "(a . b)");
  eval_text("(set-car! tmp 1)");
  check_e("tmp", "(1 . b)");
  eval_text("(set-cdr! tmp 2)");
  check_e("tmp", "(1 . 2)");

  
  check_e("(list? '(a b c))", "#t");
  check_e("(list? '())", "#t");
  check_e("(list? '(a . b))", "#f");
  check_e("(let ((x (list 'a))) (set-cdr! x x) (list? x))", "#f");


  check_e("(list 1)", "(1)");
  check_e("(list 1 2)", "(1 2)");
  check_e("(list 1 2 3)", "(1 2 3)");
  check_e("(list 'a (+ 4 3) 'c)", "(a 7 c)");
  check_e("(list)", "()");

  check_e("(list* 1)", "1");
  check_e("(list* 1 2)", "(1 . 2)");
  check_e("(list* 1 2 3)", "(1 2 . 3)");


  check_e("(length '(a b c))", "3");
  check_e("(length '(a (b) (c d e)))", "3");
  check_e("(length '())", "0");

  check_e("(append '(1 2 3))", "(1 2 3)");
  check_e("(append '(1 2 3) '(4 5 6))", "(1 2 3 4 5 6)");
  check_e("(append '(1 2 3) '())", "(1 2 3)");
  check_e("(append '() '(1 2 3))", "(1 2 3)");
  check_e("(append '(1 2 3) 'a)", "(1 2 3 . a)");
  check_e("(append '(x) '(y))", "(x y)");
  check_e("(append '(a) '(b c d))", "(a b c d)");
  check_e("(append '(a (b)) '((c)))", "(a (b) (c))");
  check_e("(append '(a b) '(c . d))", "(a b c . d)");
  check_e("(append '() 'a)", "a");
  check_e("(append '() '() '() '() 'a)", "a");

  check_e("(reverse '(a b c))", "(c b a)");
  check_e("(reverse '(a (b c) d (e (f))))", "((e (f)) d (b c) a)");

  check_e("(list-tail '(a b c d) 2)", "(c d)");

  check_e("(list-ref '(a b c d) 2)", "c");


  eval_text("(define c2r-test '((a . b) . (c . d)))");
  check_e("(caar c2r-test)", "a");
  check_e("(cdar c2r-test)", "b");
  check_e("(cadr c2r-test)", "c");
  check_e("(cddr c2r-test)", "d");

  eval_text("(define c3r-test '(((a . b) . (c . d)) . ((e . f) . (g . h))))");
  check_e("(caaar c3r-test)", "a");
  check_e("(cdaar c3r-test)", "b");
  check_e("(cadar c3r-test)", "c");
  check_e("(cddar c3r-test)", "d");
  check_e("(caadr c3r-test)", "e");
  check_e("(cdadr c3r-test)", "f");
  check_e("(caddr c3r-test)", "g");
  check_e("(cdddr c3r-test)", "h");

  eval_text("(define c4r-test"
            "'((((aa . ad) . (ba . bd)) . ((ca . cd) . (da . dd)))"
            ". (((ea . ed) . (fa . fd)) . ((ga . gd) . (ha . hd)))))");
  check_e("(caaaar c4r-test)", "aa");
  check_e("(cdaaar c4r-test)", "ad");
  check_e("(cadaar c4r-test)", "ba");
  check_e("(cddaar c4r-test)", "bd");
  check_e("(caadar c4r-test)", "ca");
  check_e("(cdadar c4r-test)", "cd");
  check_e("(caddar c4r-test)", "da");
  check_e("(cdddar c4r-test)", "dd");
  check_e("(caaadr c4r-test)", "ea");
  check_e("(cdaadr c4r-test)", "ed");
  check_e("(cadadr c4r-test)", "fa");
  check_e("(cddadr c4r-test)", "fd");
  check_e("(caaddr c4r-test)", "ga");
  check_e("(cdaddr c4r-test)", "gd");
  check_e("(cadddr c4r-test)", "ha");
  check_e("(cddddr c4r-test)", "hd");


  check_e("(memq 'a '(a b c))", "(a b c)");
  check_e("(memq 'b '(a b c))", "(b c)");
  check_e("(memq 'a '(b c d))", "#f");
  check_e("(memq (list 'a) '(b (a) c))", "#f");
  check_e("(member (list 'a) '(b (a) c))", "((a) c)");
  check_e("(memq 101 '(100 101 102))", "#f"); // unspecified
  check_e("(memv 101 '(100 101 102))", "(101 102)");


  eval_text("(define e '((a 1) (b 2) (c 3)))");
  check_e("(assq 'a e)", "(a 1)");
  check_e("(assq 'b e)", "(b 2)");
  check_e("(assq 'd e)", "#f");
  check_e("(assq (list 'a) '(((a)) ((b)) ((c))))", "#f");
  check_e("(assoc (list 'a) '(((a)) ((b)) ((c))))", "((a))");
  check_e("(assq 5 '((2 3) (5 7) (11 13)))", "#f"); // unspecified
  check_e("(assv 5 '((2 3) (5 7) (11 13)))", "(5 7)");

  return (RESULT) ? EXIT_SUCCESS : EXIT_FAILURE;
}
