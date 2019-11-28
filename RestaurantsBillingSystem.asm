INCLUDE Irvine32.inc

.DATA
bill     DWORD 0                                                      ; To store the bill...
oPrice   DWORD 169, 149, 99, 89, 69, 69, 10, 5                        ; To store the prices of Oriental...
cPrice   DWORD 169, 149, 99, 79                                       ; To store the prices of Chinese...
fPrice   DWORD 149, 99, 79, 49                                        ; To store the prices of Fast Food...
dePrice  DWORD 799, 699, 99, 69                                       ; To store the prices of Dessert...
drPrice  DWORD 99, 99, 49 ,49, 69, 64, 89, 49                         ; To store the prices of Drinks...
bool     DWORD ?                                                      ; To store the result of Check... 
filePass BYTE  15 DUP(?)                                              ; To store the Password from File...
inPass   BYTE  15 DUP(?)                                              ; To store the Input Password...

welcome  BYTE " *** Welcome To Restaurant Transylvania *** ", 0ah, 0dh, 0 ; Welcome note...

id       BYTE " Enter 1 : For Admin ", 0ah, 0dh
         BYTE " Enter 2 : For Customers ", 0ah, 0dh
	     BYTE " Enter 3 : To Exit ", 0ah, 0dh, 0

choice   BYTE " Enter 1 : To Print Sale ", 0ah, 0dh
         BYTE " Enter 2 : To Reset Password ", 0ah, 0dh
	     BYTE " Enter 3 : To Exit ", 0ah, 0dh, 0

options  BYTE " Enter 1 : To see our Menu and Prices.", 0ah, 0dh
         BYTE " Enter 2 : To see our Deals and Offers.", 0ah, 0dh
		 BYTE " Enter 3 : To Place an Order.", 0ah, 0dh
		 BYTE " Enter 4 : To Reset the Bill [Cancel the order].", 0ah, 0dh
		 BYTE " Enter 5 : To Exit.", 0ah, 0dh , 0
		                                                               ; Price Menu...
pMenu    BYTE " Restaurant Transylvania proudly present our Menu ... ", 0ah, 0dh, 0ah, 0dh
         BYTE " *** Oriental *** ", 0ah, 0dh
         BYTE "                Chicken Quorma   : 169 per Dish. ", 0ah, 0dh
         BYTE "		Pullao           : 149 per Dish. ", 0ah, 0dh
	     BYTE "		Chicken Briyani  :  99 per Dish. ", 0ah, 0dh
         BYTE "		Chicken Karahi   :  89 per Dish. ", 0ah, 0dh
         BYTE "		Chicken Tikka    :  69 per Dish. ", 0ah, 0dh
         BYTE "		Murgh Haleem     :  69 per Dish. ", 0ah, 0dh
	     BYTE "		Naan             :  10 per Piece. ", 0ah, 0dh
         BYTE "		Roti             :  05 per Piece. ", 0ah, 0dh, 0ah, 0dh
		 BYTE " *** Chinese *** ", 0ah, 0dh
         BYTE "		Chicken Manchurian with rice : 169 per Plate ", 0ah, 0dh
         BYTE "		Egg Fried Rice               : 149 per Plate ", 0ah, 0dh
         BYTE "		Chicken Macaroni             :  99 per Plate ", 0ah, 0dh
         BYTE "		Chicken Shashlik             :  79 per Plate ", 0ah, 0dh, 0ah, 0dh
         BYTE " *** Fast Food *** ", 0ah, 0dh
         BYTE "		Chicken Pizza    : 149 per Pizza. ", 0ah, 0dh
         BYTE "		Zinger Burger    :  99 per Piece. ", 0ah, 0dh
         BYTE "		Chicken Shawarma :  79 per Piece. ", 0ah, 0dh
         BYTE "		French Fries     :  49 per Packet. ", 0ah, 0dh,  0ah, 0dh
         BYTE " *** Dessert *** ", 0ah, 0dh
         BYTE "		Pineapple Cake    : 799 per Pound. ", 0ah, 0dh
         BYTE "		Chocolate Cake    : 699 per Pound. ", 0ah, 0dh
         BYTE "		Custard           :  99 per Bowl. ", 0ah, 0dh
         BYTE "		Ice-cream         :  69 per Cup. ", 0ah, 0dh, 0ah, 0dh
         BYTE " *** Drinks *** ",  0ah, 0dh
         BYTE "		Coca Cola       : 99 (1.5) Liters. ", 0ah, 0dh
         BYTE "		Sprite          : 99 (1.5) Liters. ", 0ah, 0dh
         BYTE "		Coca Cola       : 49 Regular. ", 0ah, 0dh
         BYTE "		Sprite          : 49 Regular. ", 0ah, 0dh
         BYTE "		Pineapple Juice : 69 per Glass. ", 0ah, 0dh
         BYTE "		Mint Margarita  : 64 per Glass. ", 0ah, 0dh
		 BYTE "		Coffee          : 89 per Cup. ", 0ah, 0dh
		 BYTE "		Tea             : 49 per Cup. ", 0

deals    BYTE " *** Deals and Offers *** ", 0ah, 0dh, 0                       ; TODO.......

cMenu    BYTE " *** Menu *** ", 0ah, 0dh, 0ah, 0dh                            ; Choice Menu...
         BYTE " Enter 1 : For Oriental.", 0ah, 0dh
		 BYTE " Enter 2 : For Chinese.", 0ah, 0dh
		 BYTE " Enter 3 : For Fast Food.", 0ah, 0dh
		 BYTE " Enter 4 : For Dessert.", 0ah, 0dh
		 BYTE " Enter 5 : For Drinks.", 0ah, 0dh
		 BYTE " Enter 6 : To Exit.", 0ah, 0dh, 0

oriental BYTE " *** Oriental *** ", 0ah, 0dh
         BYTE " Enter 1 : Chicken Quorma   : 169 per Dish. ", 0ah, 0dh
         BYTE " Enter 2 : Pullao           : 149 per Dish. ", 0ah, 0dh
	     BYTE " Enter 3 : Chicken Briyani  :  99 per Dish. ", 0ah, 0dh
         BYTE " Enter 4 : Chicken Karahi   :  89 per Dish. ", 0ah, 0dh
         BYTE " Enter 5 : Chicken Tikka    :  69 per Dish. ", 0ah, 0dh
         BYTE " Enter 6 : Murgh Haleem     :  69 per Dish. ", 0ah, 0dh
	     BYTE " Enter 7 : Naan             :  10 per Piece. ", 0ah, 0dh
         BYTE " Enter 8 : Roti             :  05 per Piece. ", 0ah, 0dh
		 BYTE " Enter 9 : To Exit. ", 0ah, 0dh, 0

chinese  Byte " *** Chinese *** ", 0ah, 0dh
         BYTE " Enter 1 : Chicken Manchurian with rice : 169 per Dish ", 0ah, 0dh
         BYTE " Enter 2 : Egg Fried Rice               : 149 per Dish ", 0ah, 0dh
         BYTE " Enter 3 : Chicken Macaroni             :  99 per Dish ", 0ah, 0dh
         BYTE " Enter 4 : Chicken Shashlik             :  79 per Dish ", 0ah, 0dh
		 BYTE " Enter 5 : To Exit. ", 0ah, 0dh, 0

fastFood BYTE " *** Fast Food *** ", 0ah, 0dh
         BYTE " Enter 1 : Chicken Pizza    : 149 per Pizza. ", 0ah, 0dh
         BYTE " Enter 2 : Zinger Burger    :  99 per Piece. ", 0ah, 0dh
         BYTE " Enter 3 : Chicken Shawarma :  79 per Piece. ", 0ah, 0dh
         BYTE " Enter 4 : French Fries     :  49 per Packet. ", 0ah, 0dh
		 BYTE " Enter 5 : To Exit. ", 0ah, 0dh, 0

dessert  BYTE " *** Dessert *** ", 0ah, 0dh
         BYTE " Enter 1 : Pineapple Cake    : 799 per Pound. ", 0ah, 0dh
         BYTE " Enter 2 : Chocolate Cake    : 699 per Pound. ", 0ah, 0dh
         BYTE " Enter 3 : Custard           :  99 per Bowl. ", 0ah, 0dh
         BYTE " Enter 4 : Ice-cream         :  69 per Cup. ", 0ah, 0dh
		 BYTE " Enter 5 : To Exit. ", 0ah, 0dh, 0

drinks   BYTE " *** Drinks *** ", 0ah, 0dh
	     BYTE " Enter 1 : Coca Cola       : 99 (1.5) Liters. ", 0ah, 0dh
         BYTE " Enter 2 : Sprite          : 99 (1.5) Liters. ", 0ah, 0dh
         BYTE " Enter 3 : Coca Cola       : 49 Regular. ", 0ah, 0dh
         BYTE " Enter 4 : Sprite          : 49 Regular. ", 0ah, 0dh
         BYTE " Enter 5 : Pineapple Juice : 69 per Glass. ", 0ah, 0dh
         BYTE " Enter 6 : Mint Margarita  : 64 per Glass. ", 0ah, 0dh
	     BYTE " Enter 7 : Coffee          : 89 per Cup. ", 0ah, 0dh
	     BYTE " Enter 8 : Tea             : 49 per Cup. ", 0ah, 0dh
         BYTE " Enter 9 : To Exit. ", 0ah, 0dh , 0

passWord BYTE " Enter Current Password less than 16 Characters : ", 0

newPass  BYTE " Enter New Password less than 16 Characters : ", 0

wrongPas BYTE " Password is incorrect or Input is Invalid. ", 0ah, 0dh, 0

confirm  BYTE " New Password is Set. ", 0ah, 0dh, 0

reMsg    BYTE " Your Order has been Canceled... ", 0ah, 0dh, 0

dishes   BYTE " Enter the Quantity:  ", 0

caption  BYTE "Error", 0

errMsg   BYTE " Please follow instructions correctly... ", 0

billMsg  BYTE  "    Total Bill:   Rs ", 0

exitMsg  BYTE "     Glad to have you Here... ", 0ah, 0dh, 0

.CODE
main PROC
     call crlf

	 mov edx, OFFSET welcome                                     ; Printing Welcome...
	 call writeString

     op:
	    call crlf

	    mov edx, OFFSET id                                        ; Printing Id...
		call writeString

		call crlf
		call readInt

		cmp eax, 1
		je ad

		cmp eax, 2
		je cu

		cmp eax, 3
		je  _exit

		call error                                                 ; calling error Proc...
		jmp  op

		ad:                                                        ; Admin Tag...
		   call admin
		   jmp op

		cu:
		   call customer
		   jmp op

	 _exit:
		   mov edx, OFFSET exitMsg                                 ; Printing Exit Note/Msg...
	       call writeString

	       exit
main ENDP

;-------------------------------------------------------------------
;| For admin only...                                                |
;| Uses: It deals with the admin and print whole file...            |
;| Note: It only read bill in file...                               |
;-------------------------------------------------------------------

admin PROC
       PUSHAD
	   PUSHFD

	   call crlf

	   ;TODO: Opening a file and taking input from it for pass word

		call writeString

	    mov edx, OFFSET inPass                                     ; Point to the Destination...
        mov ecx, SIZEOF inPass                                     ; Specify max characters...
		call readString

		call check

		cmp bool, 0                                                ; Check store result in bool...
		je wrong

		ok:
		   call crlf
	       mov edx, OFFSET choice
		   call writeString

		   call crlf
		   call readInt

		   cmp eax, 1
		   je  pb
	   	   cmp eax, 2
		   je  rp
		   cmp eax, 3
		   je  _exit

		   call error                                              ; calling error Proc...
		   jmp ok

		   pb:                                                     ; Print Bill Tag...
			  ;TODO: Print the whole bills
			  jmp ok
			 
		   rp:                                                     ; Reset Password Tag...
		      call crlf
		      mov edx, OFFSET passWord                             ; Taking old Password again...
		      call writeString

		      mov edx, OFFSET inPass
              mov ecx, SIZEOF inPass
		      call readString

		      call check                                           ; Rechecking Pass before Changing...

		      cmp bool, 0
		      je wrong

			  mov edx, OFFSET newPass
		      call writeString

			  mov edx, OFFSET inPass
              mov ecx, SIZEOF inPass
		      call readString                                     ; Taking new Password again...

			  cmp ecx, 15                                         ; To check our limit...
			  jg wrong

			  ; TODO: write new password in file in file....

			  call crlf
			  mov edx, OFFSET confirm
		      call writeString

			  jmp _exit

 wrong:
       call crlf
       mov edx, OFFSET wrongPas                                       ; When Password is wrong...
	   call writeString

 _exit:
	   POPFD
	   POPAD

	   RET
admin ENDP

;-------------------------------------------------------------------
;| For customers only...                                            |
;| Uses: It deals with the customers and take order...              |
;| Note: It only write bill in file with (False) customer name...   |
;-------------------------------------------------------------------

customer PROC
          PUSHAD
		  PUSHFD
		       
		  call crlf

	      op:                                                         ; Option Tag...  
		     mov edx, OFFSET options                                  ; Printing options...
	         call writeString

			 call crlf
			 call readInt

			 cmp eax, 1
			 je  pm
			 cmp eax, 2
			 je  do
			 cmp eax, 3
			 je  cm
			 cmp eax, 4
			 je  rb
			 cmp eax, 5
			 je  _exit

			 call error                                                  ; calling error Proc...
			 jmp  op

			 pm:                                                         ; Price Menu Tag...
				call printMenu

			    call crlf
				call WaitMsg                                             ; Call Wait Massage...
				call crlf

				jmp  op

			 do:                                                          ; Deals and Offers Tag...
				call dealsOffers
				jmp op

			 cm:                                                          ; Choice Menu Tag...
				call choiceMenu
				jmp op

			 rb:                                                          ; Reset Bill Tag...
				call resetBill
				call crlf
				jmp op

    _exit:                                                                ; Exit Tag
		  call printBill
		  call crlf

		  POPFD
		  POPAD

		  RET
customer ENDP

;-------------------------------------------------------------------
;| Print Menu with Prices for customers...                          |
;| Uses: pMenu string to print...                                   |
;| Note: push then pop regs and flags in stack to make them const...|
;-------------------------------------------------------------------

printMenu PROC
           PUSHAD                                                    ; Pushing all 32-bit registers...
		   PUSHFD                                                    ; Pushing all flags...

		   call crlf

		   mov edx, OFFSET pMenu
	       call writeString

		   POPFD                                                     ; Popping flags in reverse order...
		   POPAD                                                     ; Popping regs in reverse order...

		   RET
printMenu ENDP

;-------------------------------------------------------------------
;| Print Deals and Offers with Prices for customers...              |
;| Uses:   deals string to print...                                 |
;| update: bill according to selected Deals and Offers..            |
;-------------------------------------------------------------------

dealsOffers PROC
             PUSHAD
		     PUSHFD

			 call crlf

		     mov edx, OFFSET deals
	         call writeString 

		     POPFD
		     POPAD

	    	 RET
dealsOffers ENDP

;-------------------------------------------------------------------
;| Print Menu with Prices for customers to order...                 |
;| Uses:   pMenu string to print...                                 |
;-------------------------------------------------------------------

choiceMenu PROC
             PUSHAD
		     PUSHFD

			 op:                                                  ; Option Tag...
			    call crlf

		        mov edx, OFFSET cMenu
	            call writeString

		        call crlf
		        call readInt

	            cmp eax, 1
		        je  ot
		        cmp eax, 2
		        je  cn
		        cmp eax, 3
		        je  ff
		        cmp eax, 4
		        je  de
		        cmp eax, 5
		        je  dr
				cmp eax, 6
		        je  _exit

		        call error                                             ; calling error Proc...
		        jmp  op

		        ot:                                                    ; Oriental Tag...
		           call OrientalMenu
		           jmp  op

                cn:                                                    ; Chinese Tag...
	               call ChineseMenu
		           jmp op

                ff:                                                    ; Fast Food Tag...
	               call FastFoodMenu
		           jmp op

               de:                                                     ; Dessert Tag...
		          call DessertMenu
		          jmp op

		       dr:                                                     ; Drinks Tag...
	              call DrinksMenu
		          jmp op

	   _exit:                                                          ; Exit Tag
		     POPFD
		     POPAD

	    	 RET
choiceMenu ENDP

;-------------------------------------------------------------------
;| Print Oriental Menu with Prices for customers to order...        |
;| Updates: Bill ...                                                |
;-------------------------------------------------------------------

OrientalMenu PROC
			  PUSHAD
			  PUSHFD

			  op:                                                  ; Option Tag...
			     call crlf

		         mov edx, OFFSET oriental
	             call writeString

		         call crlf
		         call readInt

	             cmp eax, 1
		         je  cq
		         cmp eax, 2
		         je  pu
		         cmp eax, 3
		         je  cb
		         cmp eax, 4
		         je  ck 
		         cmp eax, 5
		         je  ct 
				 cmp eax, 6
				 je  mh
				 cmp eax, 7
				 je  na
				 cmp eax, 8
				 je  rt
				 cmp eax, 9
				 je  _exit


		         call error                                           ; calling error Proc...
		         jmp  op

		         cq:                                                  ; Chicken Quorma Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                      ; Taking input for quantity...

  				    mov ebx, [oPrice]
				    mul ebx                                           ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 pu:                                                  ; Pullao Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                      ; Taking input for quantity...

				    mov ebx, [oPrice + 4]
				    mul ebx                                           ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 cb:                                                  ; Chicken Briyani Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                      ; Taking input for quantity...

				    mov ebx, [oPrice + 8]
				    mul ebx                                           ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 ck:                                                  ; Chicken Karahi Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                      ; Taking input for quantity...

				    mov ebx, [oPrice + 12]
				    mul ebx                                           ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

		         ct:                                                  ; Chicken Tikka Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                      ; Taking input for quantity...

				    mov ebx, [oPrice + 16]
				    mul ebx                                           ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     mh:                                                  ; Murgh Haleem Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                      ; Taking input for quantity...

				    mov ebx, [oPrice + 20]
				    mul ebx                                           ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     na:                                                  ; Naan Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                      ; Taking input for quantity...

				    mov ebx, [oPrice + 24]
				    mul ebx                                           ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     rt:                                                   ; Roti Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                       ; Taking input for quantity...

				    mov ebx, [oPrice + 28]
				    mul ebx                                            ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op
	    _exit:                                                          ; Exit Tag
			  POPFD
			  POPAD

			  RET
OrientalMenu ENDP

;-------------------------------------------------------------------
;| Print Chinese Menu with Prices for customers to order...         |
;| Updates: Bill ...                                                |
;-------------------------------------------------------------------

ChineseMenu PROC
			 PUSHAD
			 PUSHFD

			 op:                                                  ; Option Tag...
			    call crlf

		        mov edx, OFFSET chinese
	            call writeString

		        call crlf
		        call readInt

	            cmp eax, 1
		        je  cm
		        cmp eax, 2
		        je  fr
		        cmp eax, 3
		        je  mn
		        cmp eax, 4
		        je  sh 
		        cmp eax, 5
				je  _exit


		        call error                                            ; calling error Proc...
		        jmp  op

		        cm:                                                   ; Chicken Manchurian Tag...
		           mov edx, OFFSET dishes
	               call writeString

				   call readInt                                       ; Taking input for quantity...

				   mov ebx, [cPrice]
				   mul ebx                                            ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

                fr:                                                   ; Egg Fried Rice Tag...
	               mov edx, OFFSET dishes
	               call writeString

				   call readInt                                       ; Taking input for quantity...

				   mov ebx, [cPrice + 4]
				   mul ebx                                            ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

                mn:                                                   ; Chicken Macaroni Tag...
	               mov edx, OFFSET dishes
	               call writeString

				   call readInt                                       ; Taking input for quantity...

				   mov ebx, [cPrice + 8]
				   mul ebx                                            ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

                sh:                                                    ; Chicken Shashlik Tag...
		           mov edx, OFFSET dishes
	               call writeString

				   call readInt                                        ; Taking input for quantity...

				   mov ebx, [cPrice + 12]
				   mul ebx                                             ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op
	   _exit:                                                          ; Exit Tag
			 POPFD
			 POPAD

			 RET
ChineseMenu ENDP

;-------------------------------------------------------------------
;| Print Fast Food Menu with Prices for customers to order...       |
;| Updates: Bill ...                                                |
;-------------------------------------------------------------------

FastFoodMenu PROC
			  PUSHAD
			  PUSHFD

			  op:                                                  ; Option Tag...
			     call crlf

		         mov edx, OFFSET fastFood
	             call writeString

		         call crlf
		         call readInt

	             cmp eax, 1
		         je  cp
		         cmp eax, 2
		         je  zb
		         cmp eax, 3
		         je  sw
		         cmp eax, 4
		         je  ff
		         cmp eax, 5
				 je  _exit


		         call error                                           ; calling error Proc...
		         jmp  op

		         cp:                                                  ; Chicken Pizza Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                      ; Taking input for quantity...

				    mov ebx, [fPrice]
				    mul ebx                                           ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 zb:                                                  ; Zinger Burger Rice Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                      ; Taking input for quantity...

				    mov ebx, [fPrice + 4]
				    mul ebx                                           ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 sw:                                                   ; Chicken Shawarma Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                       ; Taking input for quantity...

				    mov ebx, [fPrice + 8]
				    mul ebx                                            ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op
					 
                 ff:                                                    ; French Fries Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                        ; Taking input for quantity...

				    mov ebx, [fPrice + 12]
				    mul ebx                                             ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op
	    _exit:                                                          ; Exit Tag
			  POPFD
			  POPAD

			  RET
FastFoodMenu ENDP

;-------------------------------------------------------------------
;| Print Dessert Menu with Prices for customers to order...         |
;| Updates: Bill ...                                                |
;-------------------------------------------------------------------

DessertMenu PROC
			 PUSHAD
			 PUSHFD

			 op:                                                  ; Option Tag...
			    call crlf

		        mov edx, OFFSET dessert
	            call writeString

		        call crlf
		        call readInt

	            cmp eax, 1
		        je  pc
		        cmp eax, 2
		        je  cc
		        cmp eax, 3
		        je  cu
		        cmp eax, 4
		        je  ic
		        cmp eax, 5
				je  _exit


		        call error                                           ; calling error Proc...
		        jmp  op

		        pc:                                                  ; Pineapple Cake Tag...
		           mov edx, OFFSET dishes
	               call writeString

				   call readInt                                      ; Taking input for quantity...

				   mov ebx, [dePrice]
				   mul ebx                                           ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

                cc:                                                  ; Chocolate Cake Rice Tag...
	               mov edx, OFFSET dishes
	               call writeString

				   call readInt                                      ; Taking input for quantity...

				   mov ebx, [dePrice + 4]
				   mul ebx                                           ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

                cu:                                                   ; Custard Tag...
	               mov edx, OFFSET dishes
	               call writeString

				   call readInt                                       ; Taking input for quantity...

				   mov ebx, [dePrice + 8]
				   mul ebx                                            ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op
					 
                ic:                                                    ; Ice-Cream Tag...
		           mov edx, OFFSET dishes
	               call writeString

				   call readInt                                        ; Taking input for quantity...

				   mov ebx, [dePrice + 12]
				   mul ebx                                             ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op
	   _exit:                                                          ; Exit Tag
			 POPFD
			 POPAD

			 RET
DessertMenu ENDP

;-------------------------------------------------------------------
;| Print Drinks Menu with Prices for customers to order...          |
;| Updates: Bill ...                                                |
;-------------------------------------------------------------------

DrinksMenu PROC
			PUSHAD
			PUSHFD

			op:                                                  ; Option Tag...
			   call crlf

		       mov edx, OFFSET drinks
	           call writeString

		       call crlf
		       call readInt

	           cmp eax, 1
		       je  cj
		       cmp eax, 2
		       je  sj
		       cmp eax, 3
		       je  cr
		       cmp eax, 4
		       je  sr 
		       cmp eax, 5
		       je  pj
			   cmp eax, 6
			   je  ma
			   cmp eax, 7
			   je  co
			   cmp eax, 8
			   je  te
			   cmp eax, 9
			   je  _exit


		       call error                                          ; calling error Proc...
		       jmp  op

		       cj:                                                 ; Coca Cola Jumbo Tag...
		          mov edx, OFFSET dishes
	              call writeString

				  call readInt                                     ; Taking input for quantity...

  				  mov ebx, [drPrice]
				  mul ebx                                          ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

               sj:                                                 ; Sprite Jumbo Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                     ; Taking input for quantity...

				  mov ebx, [drPrice + 4]
				  mul ebx                                          ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

               cr:                                                  ; Coca Cola Regular Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                      ; Taking input for quantity...

				  mov ebx, [drPrice + 8]
				  mul ebx                                           ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

               sr:                                                  ; Sprite Regular Tag...
		          mov edx, OFFSET dishes
	              call writeString

				  call readInt                                      ; Taking input for quantity...

				  mov ebx, [drPrice + 12]
				  mul ebx                                           ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

		       pj:                                                  ; Pineapple Juice Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                      ; Taking input for quantity...

				  mov ebx, [drPrice + 16]
				  mul ebx                                           ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

			   ma:                                                  ; Mint Margarita Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                      ; Taking input for quantity...

				  mov ebx, [drPrice + 20]
				  mul ebx                                           ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

			   co:                                                  ; Coffee Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                       ; Taking input for quantity...

				  mov ebx, [drPrice + 24]
				  mul ebx                                            ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

			   te:                                                    ; Tea Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                        ; Taking input for quantity...

				  mov ebx, [drPrice + 28]
				  mul ebx                                             ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op
	  _exit:                                                          ; Exit Tag
			POPFD
			POPAD

			RET
DrinksMenu ENDP

;-------------------------------------------------------------------
;| Print a message for customers...                                 |
;| Uses:   pMenu string to print...                                 |
;| update: Reset the bill to 0...                                   |
;-------------------------------------------------------------------

resetBill PROC
           PUSHAD
		   PUSHFD

		   call crlf

		   mov edx, OFFSET reMsg
	       call writeString 

		   mov bill, 0                                                 ; Making bill 0...

		   POPFD
		   POPAD

	       RET
resetBill ENDP

;-------------------------------------------------------------------
;| Uses:   print the bill...                                        |
;-------------------------------------------------------------------

printBill PROC
           PUSHAD
		   PUSHFD

		   call crlf

		   mov edx, OFFSET billMsg
	       call writeString 

		   mov eax, bill
		   call writeInt

		   POPFD
		   POPAD

	       RET
printBill ENDP

;-------------------------------------------------------------------
;| Check the password...                                            |
;| Uses: bool variable to represent result..                        |
;| bool = 1 means True && bool = 0 means False...                   |
;-------------------------------------------------------------------

check PROC
       PUSHAD
	   PUSHFD

	   ;cmp ecx, 15                                                 ; Our Limit is of 15 Characters...
	   ;jg notEqual
;
	                            ;; lea: Load Effective Address is like combination of move and OFFSET...
	   ;lea si, filePass                                            ; ds:si points to File Password String...
       ;lea di, inPass                                              ; ds:di points to Input Password String...
       ;dec di                                                      ; Just Dec so can Inc Later...
;
       ;lab1:
            ;inc di                                                 ; Inc to get next Character...
            ;lodsb                                                  ; Load AL with next char from filePass...
                                                                   ;; note: lodsb inc si automatically...
            ;cmp [di], al                                           ; Compare characters...
            ;jne notEqual                                           ; Jump out of loop if not equal...
 ;
            ;cmp al, 0                                              ; They are the same, but end of string?
            ;jne lab1                                               ; No - so go round loop again
;
            ;mov bool, 1
	        ;jmp _exit                                              ; To save from executing notEqual Tag...
;
       ;notEqual:
	            ;mov bool, 0

 _exit:
	   POPFD
	   POPAD

	   RET
check ENDP

;-------------------------------------------------------------------
;| Shows an Error Box to customers...                               |
;| Uses:  2 strings for an input   box...                           |
;| Advan: It also works as a pause...                               |
;-------------------------------------------------------------------

error PROC
       PUSHAD
	   PUSHFD

	   call crlf

       mov ebx,OFFSET caption
	   mov edx,OFFSET errMsg
	   call MsgBox

	   POPFD
	   POPAD

	   RET
error ENDP

END main