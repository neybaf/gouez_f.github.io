function includeHTML() {
    const baseUrl = window.location.origin + '/gouez_f';  // Définir l'URL de base

    document.querySelectorAll('[data-include]').forEach(el => {
        let file = el.getAttribute('data-include');
        let filePath = new URL(file, baseUrl).href;  // Construire un chemin absolu

        fetch(filePath)
            .then(response => {
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                return response.text();
            })
            .then(data => {
                el.innerHTML = data;
            })
            .catch(error => {
                console.error('There was a problem with the fetch operation:', error);
            });
    });
}

document.addEventListener('DOMContentLoaded', includeHTML);
