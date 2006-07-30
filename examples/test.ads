# comment

diameter = Large | Small.

cheese = Mozzarella | Gorgonzola.

topping = Cheese cheese | Tomatoes | Mushrooms.

spiced = False | True.

meal =
  | WienerSchnitzel diameter
  | Pizza (diameter * spiced * topping)
  | TapirSoup spiced.

drink = Water | Beer | Wine.

satisfaction = Low | High | Very satisfaction.

domain = meal.
codomain = satisfaction.
