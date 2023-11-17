<script>
    export let title;
    export let subtitle;

    import SlideTitle from "./slideTitle.svelte";
    import Footer from "./footer.svelte";
</script>

<div class="text-left" style="height: 700px;">
    <SlideTitle {title} {subtitle}/>
    <slot />
    <Footer />
</div>