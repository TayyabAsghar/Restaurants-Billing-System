INCLUDE Irvine32.inc

.DATA
BUFFER_SIZE = 5000
PASSWORD_SIZE = 15
oPrice   DWORD 169, 149, 99, 89, 69, 69, 10, 5                    ; To store the prices of Oriental...
cPrice   DWORD 169, 149, 99, 79                                   ; To store the prices of Chinese...
fPrice   DWORD 149, 99, 79, 49                                    ; To store the prices of Fast Food...
dePrice  DWORD 799, 699, 99, 69                                   ; To store the prices of Dessert...
drPrice  DWORD 99, 99, 49 ,49, 69, 64, 89, 49                     ; To store the prices of Drinks...
bill     DWORD ?                                                  ; To store the bill...
bool     DWORD ?                                                  ; To store the result of Check... 
byteRead DWORD ?                                                  ; To store read Bytes from File...
fHandle  DWORD ?                                                  ; To store File Handle...
mockBill DWORD ?
dealRep  DWORD ?
passFile BYTE  PASSWORD_SIZE DUP(?)                               ; To store the Password from File...
userPass BYTE  PASSWORD_SIZE DUP(?)                               ; To store the Input Password...
saleFile BYTE  BUFFER_SIZE DUP(?)

welcome  BYTE " *** Welcome To Resturant Transylvania *** ", 0ah, 0dh, 0

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
         BYTE "		Chicken Cuisine              :  79 per Plate ", 0ah, 0dh, 0ah, 0dh
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
		 BYTE "		Tea             : 49 per Cup. ", 0ah, 0dh, 0

deals    BYTE " *** Deals and Offers *** ", 0ah, 0dh              ; Deals and Offers...
		 BYTE " * You will Get 5% Discount on any order above RS 1,999. * " , 0ah, 0dh
		 BYTE " For any Deal enter the related number..." , 0ah, 0dh
		 BYTE " Deal 1 : Buy any 3 or more Oriental Dishes and get 1 Dessert free. " , 0ah, 0dh
		 BYTE " Deal 2 : Buy any 2 or more Chinese Dishes and get 1 Drink free. " , 0ah, 0dh
		 BYTE " Deal 3 : Buy any 2 or more Fast Foods and get 1 Regular Drink free. " , 0ah, 0dh
		 BYTE " Deal 4 : Buy any 2 or more '1.5' Liters Drink and get 1 Regular Drink free. " , 0ah, 0dh
		 BYTE "      5 : Exit Deal Menu. ", 0ah, 0dh, 0
		 
regDrink BYTE " Enter 1 : Coca Cola       : 49 Regular. ", 0ah, 0dh
		 BYTE " Enter 2 : Sprite          : 49 Regular. ", 0ah, 0dh
		 BYTE " Enter 3 : Pineapple Juice : 69 per Glass. ", 0ah, 0dh
		 BYTE " Enter 4 : Mint Margarita  : 64 per Glass. ", 0ah, 0dh
		 BYTE " Enter 5 : To Exit. ", 0ah, 0dh , 0

drinks1_5 BYTE " Enter 1 : Coca Cola       : 99 (1.5) Liters. ", 0ah, 0dh
		 BYTE " Enter 2 : Sprite          : 99 (1.5) Liters. ", 0ah, 0dh
		 BYTE " Enter 3 : To Exit. ", 0ah, 0dh, 0

cMenu    BYTE " *** Menu *** ", 0ah, 0dh, 0ah, 0dh                ; Choice Menu...
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
         BYTE " Enter 4 : Chicken Cuisine              :  79 per Dish ", 0ah, 0dh
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

passFileName BYTE "Password.txt", 0
saleFileName BYTE "Sales.txt", 0
passWord BYTE " Enter Current Password less than 16 Characters : ", 0
newPass  BYTE " Enter New Password less than 16 Characters : ", 0
wrongPas BYTE " Password is incorrect or Input is Invalid. ", 0ah, 0dh, 0
confirm  BYTE " New Password is Set. ", 0ah, 0dh, 0
reMsg    BYTE " Your Order has been Canceled... ", 0ah, 0dh, 0    ; Reset Bill Message...
dishes   BYTE " Enter the Quantity:  ", 0
caption  BYTE " Error ", 0
errMsg   BYTE " Please follow instructions correctly... ", 0
billMsg  BYTE "    Total Bill:   Rs ", 0
exitMsg  BYTE "    Glad to have you Here... ", 0ah, 0dh, 0

dealItem BYTE " Please Select your FREE item... ", 0ah, 0dh, 0
dealAdded BYTE " Your Free item has been Added in the order Successful... ", 0ah, 0dh, 0
continueOrder BYTE " Would you like to order Something More... ", 0ah, 0dh, 0
dealCancel BYTE " You have canceled the deal... ", 0ah, 0dh, 0
discountPrice BYTE "    On Order higher than Rs 1999, Your 5% Discounted Bill is Rs : ", 0
service BYTE " *** We hope to serve you the Best *** ", 0ah, 0dh, 0
balanceMsg BYTE "    Balance is :  ", 0
amount BYTE "    Please Pay the Bill , Enter amount :  ", 0
payBill BYTE "    Please Pay the complete Bill , Enter amount :  " , 0

.CODE

dealOrientalMenu PROTO , noOfDishes:DWORD						  ; To print Oriental Menu on deals
dealChineseMenu PROTO , noOfDishes:DWORD						  ; To print Chinese Menu on deals
dealFastFoodMenu PROTO , noOfDishes:DWORD						  ; To print Fast Food Menu on deals
setEcx2 PROTO, dealQuan1:DWORD								      ; To set the value of ecx
setEcx3 PROTO, dealQuan2:DWORD							 	      ; To set the value of ecx
dealDrinks1_5 PROTO , noOfDrinks:DWORD							  ; To 1.5 liters Drink Menu on deals

main PROC
     call crlf

	 mov edx, OFFSET welcome                                      ; Printing Welcome...
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

		call error                                                ; calling error Proc...
		jmp  op

		ad:                                                       ; Admin Tag...
		   call admin
		   jmp op

		cu:
		   call customer
		   jmp op

	 _exit:
		   mov edx, OFFSET exitMsg                                ; Printing Exit Note/Msg...
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

	   call readPassword                                          ; To read Password from file...

	   cmp bool, 0
	   je _exit                               ; Bool will be 0 if something was wrong in readPassword...
	   
	   mov edx, OFFSET passWord 
	   call writeString

	   mov edx, OFFSET userPass                                   ; Point to the Destination...
       mov ecx, SIZEOF userPass                                   ; Specify max characters...
	   call readString                                            ; Take input from user...

	   call check                                                 ; Check both strings are equal...

	   cmp bool, 0                                                ; Check stores result in bool...
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
			 call readSales                                       ; To read Sales from file...
			 call crlf

			 mov edx, OFFSET saleFile
			 call writeString

			 call crlf
			 call crlf
			 call waitMsg
			 call crlf

			 jmp ok
			 
		  rp:                                                     ; Reset Password Tag...
		     call crlf
		     mov edx, OFFSET passWord                             ; Asking for old Password again...
		     call writeString

		     mov edx, OFFSET userPass
             mov ecx, SIZEOF userPass
		     call readString

		     call check                                           ; Rechecking Pass before Changing...

		     cmp bool, 0
		     je wrong

			 mov edx, OFFSET newPass
		     call writeString

			 mov edx, OFFSET userPass
             mov ecx, SIZEOF userPass
		     call readString                                      ; Taking new Password again...

			 cmp ecx, PASSWORD_SIZE                               ; To check our limit...
			 jg wrong

			 call writePassword

			 call crlf
			 mov edx, OFFSET confirm
		     call writeString

			 jmp _exit

 wrong:                                                           ; Wrong Password block...
       call crlf
       mov edx, OFFSET wrongPas                                   ; Wrong Password Message...
	   call writeString

 _exit:
	   POPFD
	   POPAD

	   RET
admin ENDP

;-------------------------------------------------------------------
;| Read sales from Sales File...                                    |
;| Uses: saleFile string to store sales...                          |
;| booL = 1 (operation succeeded) && bool = 0 (operation failed)    |
;-------------------------------------------------------------------

readSales PROC
			  PUSHAD
			  PUSHFD

			  mov edx, OFFSET saleFileName
			  call openInputFile                                  ; Opening the File...
			  mov fHandle, eax                                    ; Just for safety...      

	          mov edx, OFFSET saleFile                            ; Storage string...
	          mov ecx, BUFFER_SIZE                                ; Max buffer....
	          call ReadFromFile

	          jc err                                  ; If file does not open Carry will be set...

			  mov eax, fHandle                                    ; Moving Handel in eax...
			  call  closeFile

			  mov bool, 1                                         ; If everything is OK.
			  jmp _exit
 
			  err:                                                ; File Opening Error block... 
	              call WriteWindowsMsg
				  mov bool, 0                                     ; if does not open

	    _exit:
		      POPFD
	          POPAD

			  RET
readSales ENDP

;-------------------------------------------------------------------
;| Read password from Password File...                              |
;| Uses: passFile string to store password...                       |
;| booL = 1 (operation succeeded) && bool = 0 (operation failed)    |
;-------------------------------------------------------------------

readPassword PROC
			  PUSHAD
			  PUSHFD

			  mov edx, OFFSET passFileName
			  call openInputFile                                  ; Opening the File...
			  mov fHandle, eax                                    ; Just for safety...      

	          mov edx, OFFSET passFile                            ; Storage string...
	          mov ecx, PASSWORD_SIZE                              ; Max buffer....
	          call ReadFromFile

	          jc err                                  ; If file does not open Carry will be set...

			  mov byteRead, ecx                                   ; No of bytes read from file...
			  
			  mov eax, fHandle                                    ; Moving Handel in eax...
			  call  closeFile

			  mov bool, 1                                         ; If everything is OK.
			  jmp _exit
 
			  err:                                                ; File Opening Error block... 
	              call WriteWindowsMsg
				  mov bool, 0                                     ; if does not open

	    _exit:
		      POPFD
	          POPAD

			  RET
readPassword ENDP

;-------------------------------------------------------------------
;| Read password from Password File...                              |
;| Uses: passFile string to store password...                       |
;| booL = 1 (operation succeeded) && bool = 0 (operation failed)    |
;-------------------------------------------------------------------

writePassword PROC
			  PUSHAD
			  PUSHFD

			  mov edx, OFFSET passFileName
			  call openInputFile                                  ; Opening the File...
			  mov fHandle, eax                                    ; Just for safety...      
			  call writeint 
	          mov edx, OFFSET userPass                            ; Storage string...
	          mov ecx, PASSWORD_SIZE                              ; Max buffer....
	          call WriteToFile
			  call writeint

			  cmp eax, 0
	          jle err                                             ; If file open eax will be nonzero...
			  
			  mov eax, fHandle                                    ; Moving Handel in eax...
			  call  closeFile
			 ; call WriteWindowsMsg
			  mov bool, 1                                         ; If everything is OK.
			  jmp _exit
 
			  err:                                                ; File Opening Error block... 
	              call WriteWindowsMsg
				  mov bool, 0                                     ; if does not open

	    _exit:
		      POPFD
	          POPAD

			  RET
writePassword ENDP


;-------------------------------------------------------------------
;| Check the password...                                            |
;| Uses: bool variable to represent result..                        |
;| bool = 1 means True && bool = 0 means False...                   |
;-------------------------------------------------------------------

check PROC
       PUSHAD
	   PUSHFD

	   ;cmp ecx, byteRead
	   ;jg notEqual
;
	                            ;; lea: Load Effective Address is like combination of move and OFFSET...
	   ;lea si, passFile                                           ; ds:si points to File Password String...
       ;lea di, userPass                                           ; ds:di points to Input Password String...
       ;dec di                                                     ; Just Dec so can Inc Later...
;
       ;lab1:
            ;inc di                                                ; Inc to get next Character...
            ;lodsb                                                 ; Load AL with next char from passFile...
                                                                  ;; note: lodsb inc si automatically...
            ;cmp [di], al                                          ; Compare characters...
            ;jne notEqual                                          ; Jump out of loop if not equal...
 ;
            ;cmp al, 0                                             ; They are the same, but end of string?
            ;jne lab1                                              ; No - so go round loop again
;
            ;mov bool, 1
	        ;jmp _exit                                             ; To save from executing notEqual Tag...
;
       ;notEqual:
	            ;mov bool, 0

 _exit:
	   POPFD
	   POPAD

	   RET
check ENDP

;-------------------------------------------------------------------
;| For customers only...                                            |
;| Uses: It deals with the customers and take order...              |
;| Note: It only write bill in file with (False) customer name...   |
;-------------------------------------------------------------------

customer PROC
          PUSHAD
		  PUSHFD

	      op:                                                     ; Option Tag...  
		     call crlf

     		 mov edx, offset service                              ; Printing Service Massage...
	     	 call writeString

	     	 call crlf

			 mov edx, OFFSET options                              ; Printing options...
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

			 call error                                           ; calling error Proc...
			 jmp  op

			 pm:                                                  ; Price Menu Tag...
			    call crlf

		        mov edx, OFFSET pMenu
	            call writeString

			    call crlf
				call waitMsg                                      ; Call Wait Massage...
				call crlf

				jmp  op

			 do:                                                  ; Deals and Offers Tag...
				call dealsOffers
				jmp op

			 cm:                                                  ; Choice Menu Tag...
				call choiceMenu
				jmp op

			 rb:                                                  ; Reset Bill Tag...
				call resetBill
				jmp op

    _exit:                                                        ; Exit Tag
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
		   POPAD 

		   RET
printMenu ENDP

;-------------------------------------------------------------------
;| Print Deals and Offers with Prices for customers...              |
;| Uses:   deals string to print...                                 |
;| update: bill according to selected Deals and Offers..
;-------------------------------------------------------------------

dealsOffers PROC
             PUSHAD
		     PUSHFD
		
			 deal:
				 call crlf

				 mov edx, OFFSET deals
				 call writeString 
				 call crlf 

				 call readInt

				 cmp eax,1											; Jump to deal One d1 tag
				 je d1

				 cmp eax,2											; Jump to deal Two d2 tag
				 je d2

				 cmp eax,3
				 je d3

				 cmp eax,4
				 je d4

				 cmp eax, 5
		         je  _exit

				 call error
				 jmp deal

			 d1: 
				 mov dealRep,3								       ; Deal oriental function to add exact 3 dishes

			     invoke dealOrientalMenu, dealRep	
				 jmp _exit
			 
			  d2:

				 mov dealRep,2										;calling deal chinese function to add exact 2 dishes

			     invoke dealChineseMenu, dealRep					 
				 jmp _exit
				 	 
			 d3:

			     mov dealRep,2										;calling deal fast food function to add exact 2 dishes

			     invoke dealFastFoodMenu, dealRep					 
				 jmp _exit
	

			 d4:
				mov dealRep, 2

				invoke dealDrinks1_5, dealRep
				jmp _exit
			 
			 _exit:

			 mov edx, offset continueOrder
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

		        call error                                        ; calling error Proc...
		        jmp  op

		        ot:                                               ; Oriental Tag...
		           call OrientalMenu
		           jmp  op

                cn:                                               ; Chinese Tag...
	               call ChineseMenu
		           jmp op

                ff:                                               ; Fast Food Tag...
	               call FastFoodMenu
		           jmp op

               de:                                                ; Dessert Tag...
		          call DessertMenu
		          jmp op

		       dr:                                                ; Drinks Tag...
	              call DrinksMenu
		          jmp op

	   _exit:                                                     ; Exit Tag
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

			  op:                                                 ; Option Tag...
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

		         call error                                       ; calling error Proc...
		         jmp  op

		         cq:                                              ; Chicken Quorma Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

  				    mov ebx, [oPrice]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 pu:                                              ; Pullao Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 4]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 cb:                                              ; Chicken Briyani Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 8]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 ck:                                              ; Chicken Karahi Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 12]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

		         ct:                                              ; Chicken Tikka Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 16]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     mh:                                              ; Murgh Haleem Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 20]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     na:                                              ; Naan Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 24]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

			     rt:                                              ; Roti Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [oPrice + 28]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

	    _exit:
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


		        call error                                        ; calling error Proc...
		        jmp  op

		        cm:                                               ; Chicken Manchurian Tag...
		           mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   mov ebx, [cPrice]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

                fr:                                               ; Egg Fried Rice Tag...
	               mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   mov ebx, [cPrice + 4]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

                mn:                                               ; Chicken Macaroni Tag...
	               mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   mov ebx, [cPrice + 8]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

                sh:                                               ; Chicken Shashlik Tag...
		           mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   mov ebx, [cPrice + 12]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

	   _exit:
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

			  op:                                                 ; Option Tag...
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

		         call error                                       ; calling error Proc...
		         jmp  op

		         cp:                                              ; Chicken Pizza Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [fPrice]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 zb:                                              ; Zinger Burger Rice Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [fPrice + 4]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

                 sw:                                              ; Chicken Shawarma Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [fPrice + 8]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op
					 
                 ff:                                              ; French Fries Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

				    mov ebx, [fPrice + 12]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  op

	    _exit:
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

		        call error                                        ; calling error Proc...
		        jmp  op

		        pc:                                               ; Pineapple Cake Tag...
		           mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   mov ebx, [dePrice]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

                cc:                                               ; Chocolate Cake Tag...
	               mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   mov ebx, [dePrice + 4]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

                cu:                                               ; Custard Tag...
	               mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   mov ebx, [dePrice + 8]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op
					 
                ic:                                               ; Ice-Cream Tag...
		           mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   mov ebx, [dePrice + 12]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  op

	   _exit:
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

			op:                                                   ; Option Tag...
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

		       call error                                         ; calling error Proc...
		       jmp  op

		       cj:                                                ; Coca Cola Jumbo Tag...
		          mov edx, OFFSET dishes
	              call writeString

				  call readInt                                    ; Taking input for quantity...

  				  mov ebx, [drPrice]
				  mul ebx                                         ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

               sj:                                                ; Sprite Jumbo Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                    ; Taking input for quantity...

				  mov ebx, [drPrice + 4]
				  mul ebx                                         ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

               cr:                                                ; Coca Cola Regular Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                    ; Taking input for quantity...

				  mov ebx, [drPrice + 8]
				  mul ebx                                         ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

               sr:                                                ; Sprite Regular Tag...
		          mov edx, OFFSET dishes
	              call writeString

				  call readInt                                    ; Taking input for quantity...

				  mov ebx, [drPrice + 12]
				  mul ebx                                         ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

		       pj:                                                ; Pineapple Juice Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                    ; Taking input for quantity...

				  mov ebx, [drPrice + 16]
				  mul ebx                                         ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

			   ma:                                                ; Mint Margarita Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                    ; Taking input for quantity...

				  mov ebx, [drPrice + 20]
				  mul ebx                                         ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

			   co:                                                ; Coffee Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                    ; Taking input for quantity...

				  mov ebx, [drPrice + 24]
				  mul ebx                                         ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

			   te:                                                ; Tea Tag...
	              mov edx, OFFSET dishes
	              call writeString

				  call readInt                                    ; Taking input for quantity...

				  mov ebx, [drPrice + 28]
				  mul ebx                                         ; Mul quantity with price...
				  add eax, bill
				  mov bill, eax

		          jmp  op

	  _exit:
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

		   mov bill, 0                                            ; Making bill 0...

		   POPFD
		   POPAD

	       RET
resetBill ENDP

;-------------------------------------------------------------------
;|          |
;| Uses: Print the bill for Customers...                            |
;-------------------------------------------------------------------

discount PROC
		  PUSHAD
		  PUSHFD

		  mov eax, bill
		  cmp eax, 1999
		  jg disc
		  jmp _exit

		  disc:
			   mov ebx, 20
			   mul ebx
			   mov edx, 0
			   mov ecx, 100
			   div ecx
			   mov mockBill, eax

			   mov eax, bill
			   sub eax, mockBill
			   mov bill, eax

	_exit:
		  POPFD
		  POPAD

	      RET
discount ENDP

;-------------------------------------------------------------------
;| Uses: Print the bill for Customers...                            |
;-------------------------------------------------------------------

printBill PROC
           PUSHAD
		   PUSHFD

		   call crlf

		   mov edx, OFFSET billMsg
	       call writeString 

		   mov eax, bill
		   call writeInt                                         ; Print the original bill...
		   
		   call discount

		   call crlf

		   mov edx, offset discountPrice
		   call writeString

		   mov eax, bill
		   call writeInt										   ; Printing Bill

		   mov edx, OFFSET exitMsg                                 ; Printing Exit Note/Msg...
	       call writeString
		   call crlf

		   POPFD
		   POPAD

	       RET
printBill ENDP

;-------------------------------------------------------------------
;| Shows an Error Box to customers...                               |
;| Uses:  2 strings for an input   box...                           |
;| Advan: It also works as a pause...                               |
;-------------------------------------------------------------------

error PROC
       PUSHAD
	   PUSHFD

	   call crlf

       mov ebx, OFFSET caption
	   mov edx, OFFSET errMsg
	   call msgBox

	   POPFD
	   POPAD

	   RET
error ENDP

;-------------------------------------------------------------------
;| Print Dealed Oriental Menu with Prices for customers to order... |
;| Updates: Bill ...                                                |
;-------------------------------------------------------------------

dealOrientalMenu PROC , noOfDishes:DWORD
			  PUSHAD
			  PUSHFD

			  mov eax, bill		
			  mov mockBill, eax								      ; Stores the copy of bill				

			  mov ecx, noOfDishes

			  op:                                                 ; Option Tag...				 
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
				 je  cancelDeal

		         call error                                       ; Calling error Proc...
				 inc ecx										  ; increments ecx after 1 iteration is lost in error

		         again:
					LOOP op

				 jmp _exit

		         cq:                                              ; Chicken Quorma Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx3, eax

  				    mov ebx, [oPrice]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again

                 pu:                                              ; Pullao Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx3, eax

				    mov ebx, [oPrice + 4]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again

                 cb:                                              ; Chicken Briyani Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx3, eax

				    mov ebx, [oPrice + 8]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again

                 ck:                                              ; Chicken Karahi Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx3, eax

				    mov ebx, [oPrice + 12]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again

		         ct:                                              ; Chicken Tikka Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx3, eax

				    mov ebx, [oPrice + 16]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again

			     mh:                                              ; Murgh Haleem Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx3, eax

				    mov ebx, [oPrice + 20]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again

			     na:                                              ; Naan Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx3, eax

				    mov ebx, [oPrice + 24]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again

			     rt:                                              ; Roti Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx3, eax

				    mov ebx, [oPrice + 28]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again

		 cancelDeal:
				 mov eax, mockBill				     	          ; Canceled the order and restored the bill
				 mov bill, eax
				 mov edx, offset dealCancel
				 call writeString
				 call crlf
				 jmp finalExit

	    _exit:

				 call crlf										  ; Printing message to add a deal item in the order
				 mov edx, offset dealItem
				 call writeString
				 call crlf

				 mov edx, offset dessert						  ; Printing the free desert item menu
				 call writeString
				 call crlf
				 call readInt
				 
				 cmp eax,5										  ; Error handling for free item
				 je cancelDeal
				 cmp eax,5
				 jg selectCorrect
				 cmp eax,0
				 jle selectCorrect
				 
				 call crlf									      ; Printing success message
				 mov edx, offset dealAdded
				 call writeString
				 call crlf
				 jmp finalExit

			selectCorrect:
				 call error
				 jmp _exit

		finalExit:

			  POPFD
			  POPAD

			  RET
dealOrientalMenu ENDP

;-------------------------------------------------------------------
;| Print deal Chinese Menu with Prices for customers to order...    |
;| Updates: Bill ...                                                |
;-------------------------------------------------------------------

dealChineseMenu PROC, noOfDishes:DWORD
			 PUSHAD
			 PUSHFD

			 mov eax, bill		
			 mov mockBill, eax								      ; Stores the copy of bill		

			 mov ecx, noOfDishes
			 
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
				je  cancelDeal


		        call error                                        ; calling error Proc...
				inc ecx											  ; increments ecx after 1 iteration is lost in error

				again:
					LOOP  op
				jmp _exit

		        cm:                                               ; Chicken Manchurian Tag...
		           mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   invoke setEcx2, eax

				   mov ebx, [cPrice]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  again

                fr:                                               ; Egg Fried Rice Tag...
	               mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   invoke setEcx2, eax

				   mov ebx, [cPrice + 4]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  again

                mn:                                               ; Chicken Macaroni Tag...
	               mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   invoke setEcx2, eax

				   mov ebx, [cPrice + 8]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  again

                sh:                                               ; Chicken Shashlik Tag...
		           mov edx, OFFSET dishes
	               call writeString

				   call readInt                                   ; Taking input for quantity...

				   invoke setEcx2, eax
				   
				   mov ebx, [cPrice + 12]
				   mul ebx                                        ; Mul quantity with price...
				   add eax, bill
				   mov bill, eax

		           jmp  again
	  
	  cancelDeal:
				 mov eax, mockBill  						      ; Canceled the order and restored the bill
				 mov bill, eax
				 mov edx, offset dealCancel
				 call writeString
				 call crlf
				 jmp finalExit
	  
	  _exit:
				call crlf										  ; Printing message to add a deal item in the order
				mov edx, offset dealItem
				call writeString
				call crlf

				mov edx, offset drinks    						  ; Printing the free desert item menu
				call writeString
				call crlf
				call readInt
				 
				cmp eax,9										  ; Error handling for free item
				je cancelDeal
				cmp eax,9
				jg selectCorrect
				cmp eax,0
				jle selectCorrect

				call crlf										  ; Printing success message
				mov edx, offset dealAdded
				call writeString
				call crlf
				jmp finalExit

		
		selectCorrect:
				 call error
				 jmp _exit
			 
		finalExit:

				 POPFD
				 POPAD

			 RET
dealChineseMenu ENDP

;-------------------------------------------------------------------
;| Print deal Fast Food Menu with Prices for customers to order...  |
;| Updates: Bill ...                                                |
;-------------------------------------------------------------------

dealFastFoodMenu PROC, noOfDishes:DWORD
			  PUSHAD
			  PUSHFD

			  mov eax, bill		
			  mov mockBill, eax								      ; Stores the copy of bill	

			  mov ecx, noOfDishes

			  op:                                                 ; Option Tag...
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
				 je  cancelDeal

		         call error                                       ; calling error Proc...
				 inc ecx									      ; increments ecx after 1 iteration is lost in error

				 again:
				     LOOP op
				 jmp _exit

		         cp:                                              ; Chicken Pizza Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx2, eax

				    mov ebx, [fPrice]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again

                 zb:                                              ; Zinger Burger Rice Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx2, eax

				    mov ebx, [fPrice + 4]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again

                 sw:                                              ; Chicken Shawarma Tag...
	                mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx2, eax

				    mov ebx, [fPrice + 8]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again
					 
                 ff:                                              ; French Fries Tag...
		            mov edx, OFFSET dishes
	                call writeString

				    call readInt                                  ; Taking input for quantity...

					invoke setEcx2, eax

				    mov ebx, [fPrice + 12]
				    mul ebx                                       ; Mul quantity with price...
				    add eax, bill
				    mov bill, eax

		            jmp  again

		cancelDeal:
				   mov eax, mockBill			 				  ; Canceled the order and restored the bill
				   mov bill, eax
				   mov edx, offset dealCancel
				   call writeString
				   call crlf
				   jmp finalExit

	    _exit:
				   call crlf									  ; Printing message to add a deal item in the order
				   mov edx, offset dealItem
				   call writeString
				   call crlf
	
				   mov edx, offset regDrink    					  ; Printing the free desert item menu
				   call writeString
				   call crlf
				   call readInt
				   
				   cmp eax,5						     	      ; Error handling for free item
			       je cancelDeal
				   cmp eax,5
				   jg selectCorrect
				   cmp eax,0
				   jle selectCorrect

				   call crlf									  ; Printing success message
				   mov edx, offset dealAdded
				   call writeString
				   call crlf
				   jmp finalExit
		
		selectCorrect:
				 call error
				 jmp _exit

		finalExit:

			  POPFD
			  POPAD

			  RET
dealFastFoodMenu ENDP


dealDrinks1_5 PROC , noOfDrinks:DWORD
	
	PUSHAD
	PUSHFD
		
		mov eax, bill		
		mov mockBill, eax								          ; Stores the copy of bill	

		mov ecx, noOfDrinks

		op:
			call crlf
			mov edx, offset drinks1_5
			call writeString
			call crlf

			call readInt

			cmp eax, 1
			je drink

			cmp eax, 2
			je drink

			cmp eax, 3
			je cancelDeal

			call error
			inc ecx											      ; Inc ecx after 1 loop is lost in error

			again:
				LOOP op

			jmp _exit

			drink: 
				mov edx, OFFSET dishes
	            call writeString

				call readInt

				invoke setEcx2, eax
				call crlf

				mov ebx, offset [drPrice]
				mul ebx

				add eax,bill
				mov bill,eax
				
				jmp again

			cancelDeal:
				   mov eax, mockBill							  ; Canceled the order and restored the bill
				   mov bill, eax
				   mov edx, offset dealCancel
				   call writeString
				   call crlf
				   jmp finalExit

			_exit:
					call crlf
					mov edx, offset dealItem
					call writeString
					call crlf
	
					mov edx, offset regDrink    				  ; Printing the free desert item menu
					call writeString
					call crlf
					call readInt
					
					cmp eax,5									  ; Error handling for free item
			        je cancelDeal
				    cmp eax,5
				    jg selectCorrect
				    cmp eax,0
				    jle selectCorrect

				    call crlf									  ; Printing success message
				    mov edx, offset dealAdded
				    call writeString
				    call crlf
					jmp finalExit
		
		selectCorrect:
				 call error
				 jmp _exit
			
			finalExit:

				POPFD
				POPAD

				RET

dealDrinks1_5 ENDP

;-------------------------------------------------------------------
;| Checking number of dishes for deals 2-4                          |
;| Updates: It exit the Loop...                                     |
;-------------------------------------------------------------------

setEcx2 PROC uses eax, dealQuan1:DWORD
	    
		mov eax, dealQuan1
		cmp eax, 2
		jge setECX
				   
		jmp ignore										  ; Check the quantity of dishes to decide the value of ecx register
				   
		setECX:
		mov ecx,1

		ignore:
		RET
setEcx2 ENDP

;-------------------------------------------------------------------
;| Checking number of dishes for deal 1                             |
;| Updates: Updates: It exit the Loop...                            |
;-------------------------------------------------------------------

setEcx3 PROC uses eax, dealQuan2:DWORD
	    
		mov eax, dealQuan2
		cmp eax, 3
		jge setECX
				   
		jmp ignore										  ; Check the quantity of dishes to decide the value of ecx register
				   
		setECX:
		mov ecx,1

		ignore:
		RET
setEcx3 ENDP

END main