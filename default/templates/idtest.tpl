
{% url home a="hello" b="bla" c="ja&ja" %}
<br/>
<br/>

{{ #id }}

{% button id=#id %}

<hr/>

{% include "included.tpl" maxage=10 %}

<hr/>

<p>
{% image "koe.jpg" width=200 height=200 crop="east" mono blur style="border: 1px solid black" %}
{% image "koe.jpg" width=200 height=200 crop style="border: 1px solid black" %}
{% image "koe.jpg" width=200 height=200 crop="west" blur="20x8" style="border: 1px solid black" %}
<br/>
{% image "koe.jpg" width=200 height=200 crop="east" grey style="border: 1px solid black" %}
{% image "koe.jpg" width=200 height=200 crop style="border: 1px solid black" %}
{% image "koe.jpg" width=200 height=200 crop="west" grey blur="20x8" style="border: 1px solid black" %}
</p>

<hr/>

{% image_url "koe.jpg" width=400 crop="east" blur grey %}

<hr/>

And the id of above again: {{ #id }}