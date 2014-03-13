Title: methods
URL: methods.html
save_as: methods.html

<!--<script src="http://code.jquery.com/jquery-1.11.0.min.js"></script> -->
<script src="./jquery-1.9.1.min.js"></script>
<style>
.popup {
  display: none;
  padding-left: 40px;
  padding-right: 40px;
  border-top: 1px solid black;
  border-bottom: 1px solid black;
}

table {
  width: 100%;
}

thead {
  border-bottom: 1px solid black;
}
</style>

<script>
$(document).ready(function() {
  $('.toggleLink').click(function() {
    $(this).parents('tr').first().nextAll('.popup').first().toggle();
  });
});
</script>

## Methods

Ecologists are focussing increasingly on explaining the structure of 
communities by enriching traditional food web data with additional information. 
Many community studies collect either the food web, species' body masses, or 
abundance data, and an ever increasing number of studies measure two or three 
of these data types and/or additional data. Various combinations of data allow 
different properties to be explored and different hypotheses to be tested. 

Cheddar provides many relevant published methods. The examples below illustrate 
the different views of community data that contain different combinations of 
trophic links, body masses, and abundance data, following 
<a href="#Cohenetal2003">Cohen et al 2003</a>, without going into detail. 
Most examples use the dataset of Tuesday Lake sampled in 1984 
(<a href="#Cohenetal2003">Cohen et al 2003</a>; 
 <a href="#Jonssonetal2005">Jonsson et al 2005</a>).

<table>
  <thead>
    <tr>
      <th style="width: 80px">Trophic links</th>
      <th style="width: 80px">Body mass</th>
      <th style="width: 80px">Numerical abundance</th>
      <th>Description</th>
    </tr>
  </thead>

{% for method in methods %}
  <tr>
    <td style="text-align: center">{{ '&check;' if 'TRUE'==method.links }}</td>
    <td style="text-align: center">{{ '&check;' if 'TRUE'==method.M }}</td>
    <td style="text-align: center">{{ '&check;' if 'TRUE'==method.N }}</td>
    <td><a href="javascript:;" class="toggleLink">{{ method.description }}</a></td>
  </tr>
  <tr class="popup">
    <td colspan="4" style="max-width: 720px">
      <p>
        <strong><em>Functions</em></strong>:
        {% for f in method.functions.split(' ') %}
          <code>{{ f }}</code>
        {% endfor %}
      </p>

      {% if method.references %}
        <p>
          <strong><em>References</em></strong>:
          {% for f in method.references.split(';') %}
            <a href="#{{ f.strip().replace(' ','') }}">{{ f }}</a>;
          {% endfor %}
        </p>
      {% endif %}
     
      <p><strong><em>Examples</em></strong></p>

      {% if method.graphical1 %}
        <!--{% highlight 'r' %}{{ method.graphical1 }}{% endhighlight %}-->
        {% set path = 'output/' + method.safename + 'graphical1.txt' %}
        {% highlight 'rout' %}{% include path %}{% endhighlight %}

        <img src="/static/images/{{ method.safename }}1.png" 
             alt="Example {{ method.description }} plot"/>
      {% endif %}

      {% if method.graphical2 %}
        <!--{% highlight 'r' %}{{ method.graphical2 }}{% endhighlight %}-->
        {% set path = 'output/' + method.safename + 'graphical2.txt' %}
        {% highlight 'rout' %}{% include path %}{% endhighlight %}

        <img src="/static/images/{{ method.safename }}2.png" 
             alt="Example {{ method.description }} plot"/>
      {% endif %}

      {% if method.textual1 %}
        <!--{% highlight 'r' %}{{ method.textual1 }}{% endhighlight %}-->
        {% set path = 'output/' + method.safename + 'textual1.txt' %}
        {% highlight 'rout' %}{% include path %}{% endhighlight %}
      {% endif %}
    </td>
  </tr>
{% endfor %}
</table>

## References
{% for reference in references %}
<a href="{{ reference.url }}" name="{{ reference.id.strip().replace(' ','') }}">
  {{ reference.ref }}
</a>
{% endfor %}
