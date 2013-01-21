{% extends "admin_base.tpl" %}

{% block title %}{_ Status _}{% endblock %}

{% block content %}

<div class="edit-header">
    <h2>{_ System Status _}</h2>

</div>
<div>
    <h3>{{ m.session_manager.session_count }} {_ Active Sessions _}</h3>

    {% for s in m.session_manager %}
        Session ({{ m.session_manager.session[s].auth_user_id.title }}):
        {{ s|pprint }}<br />
        Props: {{ m.session_manager.session[s]|make_list|pprint }}<br />
        {% for p in m.session_manager.session[s].pages %}
            Page: {{ p|pprint }}<br />
            Props: {{ m.session_manager.page[p]|make_list|pprint }}<br />
        {% endfor %}
        <hr />
    {% endfor %}
</div>

{#
{% debug %}
#}

{% endblock %}
