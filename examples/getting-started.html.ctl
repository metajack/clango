<!DOCTYPE html>
<html>
  <head>
    <title>{{ title }}</title>
  </head>
  <body>
    <div id="news">
      {% for a in articles %}
      <h1>{{ a.title }}</h1>
      {{ a.content }}
      {% endfor %}
    </div>
  </body>
</html>
