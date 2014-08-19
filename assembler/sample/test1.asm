
	;; this is a comment line (test comment)
.main:	; test label
	;; this is a comment line (test comment)
	call .main + 1		; test offset
	mvi A, 12		; test register/dec literal
	mvi B, 10h		; test register/hex literal
	mov A, B		; test registers
	call 0012h		; test address
	xchg			; test no arg

end:				; test another label
	aci 10h
	nop
	hlt
	hlt
	hlt
