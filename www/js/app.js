

// ============================================================
// app.js - JavaScript para Turismo Galaxy
// ============================================================

// ============================================================
// NAVBAR - Efecto de scroll
// ============================================================
document.addEventListener('DOMContentLoaded', function() {
  
  // Obtener la navbar
  var navbar = document.querySelector('.navbar');
  
  // Función para manejar el scroll
  function handleScroll() {
    if (window.scrollY > 50) {
      navbar.classList.add('scrolled');
    } else {
      navbar.classList.remove('scrolled');
    }
  }
  
  // Escuchar evento scroll
  window.addEventListener('scroll', handleScroll);
  
  // Ejecutar al cargar para verificar posición inicial
  handleScroll();
  
  // ============================================================
  // SCROLL SUAVE PARA ENLACES INTERNOS
  // ============================================================
  document.querySelectorAll('a[href^="#"]').forEach(function(anchor) {
    anchor.addEventListener('click', function(e) {
      var href = this.getAttribute('href');
      if (href !== '#') {
        e.preventDefault();
        var target = document.querySelector(href);
        if (target) {
          target.scrollIntoView({
            behavior: 'smooth',
            block: 'start'
          });
        }
      }
    });
  });
  
  // ============================================================
  // ANIMACIÓN DE CONTADORES (opcional)
  // ============================================================
  // Si quieres animar los números en la sección de estadísticas
  // puedes descomentar esta sección
  
  /*
  function animateCounter(element, target) {
    var current = 0;
    var increment = Math.ceil(target / 50);
    var timer = setInterval(function() {
      current += increment;
      if (current >= target) {
        current = target;
        clearInterval(timer);
      }
      element.textContent = current;
    }, 30);
  }
  
  // Observar cuando la sección de estadísticas es visible
  var statsSection = document.querySelector('.stats-banner');
  if (statsSection) {
    var observer = new IntersectionObserver(function(entries) {
      entries.forEach(function(entry) {
        if (entry.isIntersecting) {
          var numbers = document.querySelectorAll('.stat-number');
          numbers.forEach(function(stat) {
            var text = stat.textContent;
            var number = parseInt(text.replace(/[+]/g, ''));
            if (!isNaN(number)) {
              stat.textContent = '0';
              animateCounter(stat, number);
            }
          });
          observer.unobserve(entry.target);
        }
      });
    });
    observer.observe(statsSection);
  }
  */
  
  console.log('🚀 Turismo Galaxy - App cargada correctamente');
});