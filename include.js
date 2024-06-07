function includeHTML() {
    let elements = document.querySelectorAll('[data-include]');
    elements.forEach(el => {
        let file = el.getAttribute('data-include');
        fetch(file)
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