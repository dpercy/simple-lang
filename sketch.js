
function sketch(v) {
    if (Array.isArray(v)) {
        return '[' + v.map(sketch).join(', ') + ']';
    } else if (typeof v === 'object' && 'sketch' in v) {
        return v.sketch();
    } else {
        return JSON.stringify(v);
    }
}

module.exports = {
    sketch,
};
