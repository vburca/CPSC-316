; 2.63

; a. The two procedures produce the same result for every tree,
; because they are different implementations of the same algorithm:
; first we go all the way on the left branch of a node, then we append 
; the node, and then we go all the way on the right branch of the node.
; (in-order traversal algorithm). Therefore, for the given trees in
; figure 2.16, both procedures will produce the following list:
; ( 1 3 5 7 9 11 )

; b. tree->list-1 is O(n*log n): on each recursive call, it has to visit
;      each element (therefore, in the end it will visit all n elements); 
;      on each visit, it calls cons and append on the left and right branch;
;      cons is O(1) growth factor; append depends (is proportional to) 
;      on the size of the first argument, which here is left-branch; since 
;      the tree is balanced, the left-branch will represent a factor of 
;      log n. Therefore, the total growth factor is O(n*log n).
;
;   tree->list-2 is O(n): also visiting each node per iteration (n total
;      visits); but this time, this one only calls cons, which is O(1) 
;      growth factor. Therefore, the total growth factor is O(n).

; 2.64

; a. The partial-tree procedure divides the sequence of n 
;       elements into 3 parts -
;       it constructs a tree (left-tree) from the left half
;       of the given list; gets this-entry, which is the
;       middle element of the given n - sequence, and then
;       constructs the right-tree from the remaining non-left-elements
;       which are the ones remaining in the n - sequence, after
;       extracting the left half and the middle element.
;       Finally, it constructs the partial tree by placing 
;       this-entry (middle one) as the node, left-tree as the
;       left branch and right-tree as the right branch.
;     Produced result for ( 1 3 5 7 9 11 ):
;                   5
;                  /  \ 
;                 1    9
;                  \   /\
;                   3 7  11

; b. The growth in number is O(n):
;        - on each side of the list (left-tree, right-tree),
;          there are n/2 operations -> for splitting the list
;          further and for computing the new right-size; these
;          operations are constant, O(1). 
;        - therefore, since we have to do this for both sides,
;          the total growth in number is O(n).

; 2.65

; help procedures from book
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; procedures defined in class
(define (member? element set)
  (cond ((null? set) #f)
        ((equal? element (car set)) #t)
        (else (member? element (cdr set)))))

(define (adjoin element set)
  (if (member? element set)
      set
      (cons element set)))

(define (filter p l)
  (cond ((null? l) '())
        ((p (car l)) (cons (car l) (filter p (cdr l))))
  (else (filter p (cdr l)))))

(define (union set1 set2)
  (append (filter (lambda (element) (not (member? element set2))) set1) set2))

(define (intersection set1 set2)
  (filter (lambda (element) (member? element set2)) set1))

; union and intersection set procedures
(define (union-set set-a set-b)
  (list->tree (union (tree->list-1 set-a) (tree->list-1 set-b)))
  )

(define (intersection-set tree1 tree2)
  (list->tree (intersection (tree->list-1 tree1)(tree->list-1 tree2))))
      

; 3.2

(define (make-monitored f)
  (define count 0)
  (define (mf command)
    (cond ((eq? command 'how-many-calls?) count)
          ((eq? command 'reset-count) (set! count 0))
          (else (set! count (+ 1 count)) (f command))
          )
    )
  mf
  )

; 3.3

(define (make-account balance password)
  (define saved-password password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch passw m)
    (if (eq? passw saved-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (error) "Incorrect Password"))
    )   
  dispatch)

; 3.8

(define f
  (let ((multiplier (- 1)) (count 0))
    (lambda (x) 
      (if (= count 2)
          (begin
            (set! multiplier (- 1))
            (set! count 0)
            )
          (set! count (+ count 1))
          )
      (if (= multiplier (- 1))
          (set! multiplier x) 
          )
      (* multiplier x)
      )
    )
  )