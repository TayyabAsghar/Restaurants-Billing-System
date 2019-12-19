# <u>RestaurantsBillingSystem</u>

This is a basic restaurants billing system in Assembly Language as part of my lab project.

It has a starter Starter Menu for Admin [Owner] and customer:

- Admin
- Customer
- Exit program

## <u>Admin</u>

He should `provide his password to continue`.
Old password is collected from a password file.

Once enters it has following Menu:

- To Print Sale
- To Change Password
- To Exit

### <u>Print Sale</u>

It prints the whole file of sales to show the owner.

### <u>Change Password</u>

It again asks for `old password` before asking `new password`. Once old password is checked then users enter the new password and it is stored in the password file. Then the program is moved to Main Menu.

## <u>Customer</u>

It's Starter Menu gives following options:

- To see Menu and Prices
- To see Deals and Offers
- To Place an Order
- To Reset the Bill [Cancel the order]
- To Exit [Terminate Program]

### <u>Menu and Prices</u>

It only prints the complete Menu with prices to help our customers in selection.

### <u>Deals and Offers</u>

It tells about Deals and Offers with following Menu:

- You will Get 5% Discount on any order above RS 1,999
- Deal 1 : Buy any 3 or more Oriental Dishes and get 1 Dessert free
- Deal 2 : Buy any 2 or more Chinese Dishes and get 1 Drink free
- Deal 3 : Buy any 2 or more Fast Foods and get 1 Regular Drink free
- Deal 4 : Buy any 2 or more '1.5' Liters Drink and get 1 Regular Drink free
- 5 : Exit Deal Menu

Each deal is ended by a Free Menu.

- Coca Cola (1.5) Liters
- Sprite (1.5) Liters
- To Exit

### <u>Place an Order</u>

It Places the order of customer by showing different dishes and their prices as follow:

- Oriental

  + Chicken Quorma
  + Pullao
  + Chicken Briyani
  + Chicken Karahi
  + Chicken Tikka
  + Murgh Haleem
  + Naan
  + Roti

- Chinese

  + Chicken Manchurian with rice
  + Egg Fried Rice
  + Chicken Macaroni
  + Chicken Cuisine

- Fast Food

  + Chicken Pizza
  + Zinger Burger
  + Chicken Shawarma
  + French Fries

- Dessert

  + Pineapple Cake
  + Chocolate Cake
  + Custard
  + Ice-cream

- Drinks

  + Coca Cola
  + Sprite
  + Coca Cola [Regular]
  + Sprite [Regular]
  + Pineapple Juice
  + Mint Margarita
  + Coffee
  + Tea

After selecting the dish the program asks for quantity and generates a bill for customers.

### <u>Reset the Bill [Cancel the order]</u>

It clears the bill and cancel the whole order.
