#lang racket
;TDA
;CONSTRUCTORES
;representación: letter(String) x name(String) x capacity(int) x content(list)
;Crea el drive, recive una letra, nombre y capacidad del drive a crear y los transforma en lista
;Dom: por lambda -> letter(string) X name(string) X capacity(int)
;Rec: list
(define make-drive
  (lambda (letter name capacity)
    (list letter name capacity)))
;representación: name(string) X users(name) X drives
(define make-system
  (lambda (name users drives current-user current-drive current-path)
    (list name users drives current-user current-drive current-path)))

;--------------------------------------------------

;SELECTORES
;Obtiene el primer elemento, la letra del drive
(define get-drive-letter car)
;obtiene el segundo elemento de la lista, el nombre del drive
(define get-drive-name cadr)
;obtiene el tercer elemento de la lista, la capacidad del drive
(define get-drive-capacity caddr)

;----------------------------------------------

;funcion que obtiene el nombre del sistema, que es el primer elemento ded la lista
(define get-system-name car)  ;System -> String

;funcion que obitene a los usuarios, que son listas dentro deel sistema, sin contar la primera posicion
(define get-system-users cadr) ;System -> String List

;obtiene los drives deel sistema aplicando un car y cdr, genera una lista
(define get-system-drives caddr) ;System -> Drive List

;obtiene el nombre de usuario actual, en la 4ta posicion de la lista, es un string
(define get-system-current-user cadddr) ;System -> String

;Obtiene el drive actual en el que se encuentra en el sistema
(define get-system-current-drive
  (lambda (system)
    (car (cdr (cdr (cdr (cdr system))))))) ;System -> char
(define get-system-current-path (lambda (system) (car (cdr (cdr (cdr (cdr (cdr system)))))))) ;System -> String

;--------------------------------------------

;; Modificadores
;; agregar nuevo drive
;Dom: system string
;Rec: lista actualizada
(define system-add-drive
  (lambda (system new-drive)
    (make-system (get-system-name system)
                 (get-system-users system)
                 (cons new-drive (get-system-drives system))
                 (get-system-current-user system)
                 (get-system-current-drive system)
                 (get-system-current-path system))))

;; agregar nuevo drive
(define system-add-user
  (lambda (system new-user)
    (make-system (get-system-name system)
                 (cons new-user (get-system-users system))
                 (get-system-drives system)
                 (get-system-current-user system)
                 (get-system-current-drive system)
                 (get-system-current-path system))))

;; Capa Pertenencia TDA System
;; member verifica si existe un elemento de una lista, true si existe
(define (exists-system-drive? letter system)
  (member letter (map get-drive-letter (get-system-drives system))))

(define (exists-system-user? user system)
  (member user (get-system-users system)))


;Dominio: string con el nombre del sistema
;Recorrido: lista, el primer elemento de la lista es el nombre otorgado
(define (system name)
  (list name '() '() "" #\0 ""))


;Funcion para hacer correr un comando en el sistema
;Dom: system string y cmd funcion
;Rec: llama a la funcion correspondiente, system
(define (run system cmd)
  (cmd system))

;Funcion para crear un nuevo drive en el sistema
;Dom: system string X name string X capacity int
;Rec: system, genera la nueva lista con el nuevo drive agregado
(define add-drive
  (lambda (system)
    (lambda (letter name capacity)
      (if (not (exists-system-drive? letter system)) ;; la letra de la unidad es única, no debo agregar una leta que ya exista
          (system-add-drive system    ;;if true then create a new system with the drive
                            (make-drive letter name capacity))
          system)))) ;;else return system

;Funcion para agregar un nuevo usuario al sistema
;; Dom: System X username (str)
;; Rec: System
(define add-user
  (lambda (system)
    (lambda (username)
      (if (not (exists-system-user? username system)) ;; si usuario no existe, entonces agregar
          (system-add-user system username) ;; retornar sistema
          system)))) ;; si usuario existe, retornar sistema sin cambios


;Creacion del sistema
(define S0 (system "newSystem"))
S0
;añadiendo unidades. Incluye caso S2 que intenta añadir unidad con una letra que ya existe
(define S1 ((run S0 add-drive) #\C "SO" 1000))
S1
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
S2
(define S3 ((run S2 add-drive) #\D "Util" 2000))
S3
;añadiendo usuarios. Incluye caso S6 que intenta registrar usuario duplicado
(define S4 ((run S3 add-user) "user1"))
S4
(define S5 ((run S4 add-user) "user1")) ;; retorna system actual sin cambios
S5
(define S6 ((run S5 add-user) "user2"))
S6