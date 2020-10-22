# Haskell_Hex_Game
### Реализация игры Гекс на ЯП Haskell.

### Описание : https://en.wikipedia.org/wiki/Hex_(board_game)

### Правила :

•	В гекс играют на специальной доске, которая состоит из шестиугольных полей. Доска может быть любого размера и различных форм, но традиционно используют доску в форме ромба размера n×n, чаще всего 11×11; другие популярные размеры доски 14×14 и 19×19. Джон Нэш, один из изобретателей игры, согласно биографической книге «Игры разума» Сильвии Назар, считал оптимальным размер доски 14×14. На ромбической доске каждое поле в центре доски граничит с шестью соседними полями, поля на сторонах доски имеют четверых соседей, угловые поля граничат с 2 или 3 полями.

•	Один из игроков играет красными фишками (или по-другому камнями), другой — синими (другие варианты раскраски фишек — чёрные и белые, синие и жёлтые, и т. д.). Каждый игрок поочерёдно ставит фишку своего цвета на любое свободное поле. Начинают синие.

•	Две противоположные стороны доски окрашены в красный и синий цвета, и называются соответственно красной и синей сторонами. Поля на углах доски относятся к обеим сторонам. Для того чтобы выиграть, игрок должен выстроить цепь из своих фишек, соединив ею стороны своего цвета, то есть красные стремятся построить цепь из красных фишек между двумя красными сторонами доски, а синие — цепь из синих фишек между синими сторонами.

• У первого игрока всегда есть победная стратегия, так как лишний ход не может помешать построить цепь. Для уравнивания шансов используется «Правило пирога», которое позволяет второму игроку сменить цвет сразу после того, как первый игрок делает свой первый ход. При игре с «правилом пирога» выигрышная стратегия есть у второго игрока.
