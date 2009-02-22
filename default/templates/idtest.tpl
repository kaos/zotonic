
{% url home a="hello" b="bla" c="ja&ja" %}
<br/>
<br/>

{{ #id }}

{% @button id=#id %}

<hr/>

{% @include file="included.tpl" maxage=10 %}

<hr/>
And the id of above again: {{ #id }}