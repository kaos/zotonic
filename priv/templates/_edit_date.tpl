{% ifequal date|date:'Y' 9999 %}
<input type="text" style="width:32px" name="dt:y:{{ is_end }}:{{ name }}" value="" />
<input type="text" style="width:20px" name="dt:m:{{ is_end }}:{{ name }}" value="" />
<input type="text" style="width:20px" name="dt:d:{{ is_end }}:{{ name }}" value="" /> &mdash;
<input type="text" style="width:20px" name="dt:h:{{ is_end }}:{{ name }}" value="" />:<input type="text" style="width:20px" name="dt:i:{{ is_end }}:{{ name }}" value="" />
{% else %}
<input type="text" style="width:32px" name="dt:y:{{ is_end }}:{{ name }}" value="{{ date|date:'Y' }}" />
<input type="text" style="width:20px" name="dt:m:{{ is_end }}:{{ name }}" value="{{ date|date:'m' }}" />
<input type="text" style="width:20px" name="dt:d:{{ is_end }}:{{ name }}" value="{{ date|date:'d' }}" /> &mdash;
<input type="text" style="width:20px" name="dt:h:{{ is_end }}:{{ name }}" value="{{ date|date:'H' }}" />:<input type="text" style="width:20px" name="dt:i:{{ is_end }}:{{ name }}" value="{{ date|date:'i' }}" />
{% endifequal %}
