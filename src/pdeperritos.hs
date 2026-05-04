type raza = String
type jugueteFav = String
type tiempoEnGuarderia = String
type energia = String

type perro = (raza, [jugueteFav], tiempoEnGuarderia, energia)

type actividad = String
type duracion = Int
type rutina = (actividad, duracion)

Raza :: perro -> raza
Raza (r,_,_,_) = r

Juguetes :: perro -> [jugueteFav]
Juguetes (_,[j],_,_) = [j]

TiempoDePermanencia :: perro -> duracion
TiempoDePermanencia (_,_,t,_) = t 

Energia :: perro -> energia
Energia (_,_,_,e) = e 

GuarderiaPdePerritos :: rutina
GuarderiaPdePerritos = [("jugar", 30)
                      , ("ladrar 18", 20)
                      , ("regalar", 0)
                      , ("diaDeSpa", 120)
                      , ("diaDeCampo", 720)]

TiempoTotalRutina :: rutina -> tiempoEnGuarderia
TiempoTotalRutina = sum . map snd

esRazaExtravagante :: raza -> Bool
esRazaExtravagante r = r == "dalmata" || r == "pomerania"

# 1
PuedeEstar :: perro -> Bool
PuedeEstar Perro = TiempoDePermanencia Perro > TiempoTotalRutina GuarderiaPdePerritos

# 2 perros responsables
esPerroResponsable :: perro -> Bool
esPerroResponsable Perro = lenght (Juguetes Perro) > 3

AplicarActividad :: (actividad, duracion) -> perro -> perro
AplicarActividad ("jugar",_) perro = let (r, j, t, e) = perro in (r,j,t,max 0 (e -10))

AplicarActividad ("ladrar 18",_) perro = let (r, j, t, e) = perro in (r, j, t, e + 9)  -- *mitad de 18 = 9*

AplicarActividad ("regalar pelota",_) perro = let (r, j, t, e) = perro in (r, "pelota": j, e )

Aplicar Actividad ("diaDeSpa",_) perro = let (r, j, t, e) = perro in if TiempoDePermanencia perro>= 50 || esRazaExtravagante r
                                         then (r, "peine de goma" : j, t, e)

AplicarActividad ("diaDeCampo") perro = let (r, j, t, e) = in if null j then perro else (r, tail : j, t, e)  -- significa que pierde el primer juguete

AplicarRutina :: rutina -> perro -> perro
aplicarRutina rutina perro = foldl (flip AplicarActividad) rutina perro
