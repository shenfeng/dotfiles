# wmname LG3D
xset r rate 250 60 # faster keyboard
xset b off  # disable bell

xmodmap -e 'keycode 66 = Control_L'
xmodmap -e 'clear Lock'
xmodmap -e 'add Control = Control_L'

xrdb -load ~/.Xresources

wmname LG3D
exec awesome
