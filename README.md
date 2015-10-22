##Lager_replace
Lager_replace это специальное приложение, которое занимается подменной log вызовов Lager, на log вызовы Chronica. 
Дял всех остальных вызовов устанавливаются заглушки. 

##Использование
* Для того, чтобы начать работу с Lager_replace, нужно указать его в зависимостях rebar.config вместо Lager
Пример:
```erlang
{deps, [
    {lager, ".*", {git, "git@github.com:eltex-ecss/lager_replace.git"}}
]}.
```
<br>
* Настроить sys.config, в соотвествии с ниже указынными данными
```erlang
Tag :: atom().
Tags :: [Tag()].
Level1 :: debug | trace | info | warning | alert | error | critical | emergency.
Level2 :: debug | trace | info | warning | error.

lager:Level1("String")                =>  log:Level2([lager_Level1], "String") 
lager:Level1("Format", [Args])        =>  log:Level2([lager_Level1], "Format", [Args])
lager:Level1(Tag, "Format", [Args])   =>  log:Level2([Tag, lager_Level1], "Format", [Args])
lager:Level1(Tags, "Format", [Args])  =>  log:Level2([Tags, lager_Level1], "Format", [Args])

lager:(emergency, critical, error)    = log:error
lager:(alert, warning)                = log:warning
lager:info                            = log:info
lager:trace                           = log:trace
lager:debug                           = log:debug
```
